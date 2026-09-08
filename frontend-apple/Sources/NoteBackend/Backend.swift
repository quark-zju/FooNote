import Foundation
import CNoteBackend

public struct NodeID: Hashable, Sendable {
    public let backend: Int32
    public let id: Int32

    public init(backend: Int32, id: Int32) {
        self.backend = backend
        self.id = id
    }
}

public enum BackendError: Error, LocalizedError, Equatable {
    case closed
    case operationFailed(code: Int32, message: String)
    case malformedResponse

    public var errorDescription: String? {
        switch self {
        case .closed:
            return "The backend is closed."
        case let .operationFailed(_, message):
            return message
        case .malformedResponse:
            return "The backend returned a malformed response."
        }
    }
}

@MainActor
public final class Backend {
    private var isOpen = false

    public init() {}

    @discardableResult
    public func open(_ url: String) throws -> NodeID {
        try transaction {
            push(url)
            try check(notebackend_open_root_url())
            let root = try popNodeID()
            isOpen = true
            return root
        }
    }

    public func children(_ node: NodeID) throws -> [NodeID] {
        try requireOpen {
            return try transaction {
                push(node)
                try check(notebackend_get_children())
                let count = Int(try popInt())
                guard count >= 0 else { throw BackendError.malformedResponse }
                var result: [NodeID] = []
                result.reserveCapacity(count)
                for _ in 0..<count {
                    result.append(try popNodeID())
                }
                return result.reversed()
            }
        }
    }

    public func text(_ node: NodeID) throws -> String {
        try requireOpen {
            try transaction {
                push(node)
                try check(notebackend_get_text())
                return try popString()
            }
        }
    }

    @discardableResult
    public func insert(parent: NodeID, text: String, meta: String = "", position: Int32 = 0) throws -> NodeID {
        try requireOpen {
            try transaction {
                push(parent)
                notebackend_stack_push_i32(position)
                push(text)
                push(meta)
                try check(notebackend_insert())
                return try popNodeID()
            }
        }
    }

    public func autofill(_ node: NodeID) throws {
        try requireOpen {
            try transaction {
                push(node)
                try check(notebackend_autofill())
            }
        }
    }

    public func metadata(_ node: NodeID) throws -> String {
        try requireOpen {
            try transaction {
                push(node)
                try check(notebackend_get_raw_meta())
                return try popString()
            }
        }
    }

    public func extractMeta(_ node: NodeID, prefix: String) throws -> String {
        try requireOpen {
            try transaction {
                push(node)
                push(prefix)
                try check(notebackend_extract_meta())
                return try popString()
            }
        }
    }

    public func updateMeta(_ node: NodeID, prefix: String, value: String) throws {
        try requireOpen {
            try transaction {
                push(node)
                push(prefix)
                push(value)
                try check(notebackend_update_meta())
            }
        }
    }

    /// Produce a standalone recovery file while preserving encrypted mount
    /// ciphertext. The backend remains open and unchanged apart from syncing
    /// inlined mount data into its source text.
    public func exportSnapshot() throws -> Data {
        try requireOpen {
            try transaction {
                try check(notebackend_export_snapshot())
                return try popBytes()
            }
        }
    }

    public func parent(_ node: NodeID) throws -> NodeID {
        try requireOpen {
            try transaction {
                push(node)
                try check(notebackend_get_parent())
                return try popNodeID()
            }
        }
    }

    public func setText(_ node: NodeID, text: String) throws {
        try requireOpen {
            try transaction {
                push(node)
                push(text)
                try check(notebackend_set_text())
            }
        }
    }

    public func remove(_ node: NodeID) throws {
        try requireOpen {
            try transaction {
                push(node)
                try check(notebackend_remove())
            }
        }
    }

    public func remove(_ nodes: [NodeID]) throws {
        try requireOpen {
            try transaction {
                pushNodeList(nodes)
                try check(notebackend_remove_batch())
            }
        }
    }

    @discardableResult
    public func move(_ nodes: [NodeID], destination: NodeID, position: Int32) throws -> [NodeID] {
        try requireOpen {
            try validateMove(nodes, destination: destination, position: position)
            return try transaction {
                pushNodeList(nodes)
                push(destination)
                notebackend_stack_push_i32(position)
                try check(notebackend_set_parent_batch())
                return try popNodeList()
            }
        }
    }

    /// Validate every selected node before invoking Rust's sequential batch move.
    /// This prevents a multi-head request from partially moving before a later
    /// cyclic item is rejected by the backend.
    private func validateMove(_ nodes: [NodeID], destination: NodeID, position: Int32) throws {
        let target = position == 0 ? destination : try parent(destination)
        for node in nodes {
            var current = target
            var seen: Set<NodeID> = []
            while seen.insert(current).inserted {
                if current == node {
                    throw BackendError.operationFailed(code: 22, message: "Cannot move a node below its descendant.")
                }
                let next = try parent(current)
                if next == current { break }
                current = next
            }
        }
    }

    /// Starts a background search. Poll `searchResults()` and `searchComplete()` from the UI.
    public func searchStart(_ query: String, roots: [NodeID]) throws {
        try requireOpen {
            try transaction {
                push(query)
                pushNodeList(roots)
                try check(notebackend_search_start())
            }
        }
    }

    public func searchResults() throws -> [(NodeID, String)] {
        try requireOpen {
            try transaction {
                notebackend_stack_push_i32(0)
                try check(notebackend_search_result())
                let count = Int(try popInt())
                guard count >= 0 else { throw BackendError.malformedResponse }
                var result: [(NodeID, String)] = []
                result.reserveCapacity(count)
                for _ in 0..<count {
                    let line = try popString()
                    let node = try popNodeID()
                    result.append((node, line))
                }
                return result
            }
        }
    }

    public func searchComplete() throws -> Bool {
        try requireOpen {
            try transaction {
                try check(notebackend_search_is_complete())
                return try popInt() != 0
            }
        }
    }

    public func searchStop() throws {
        try requireOpen {
            try transaction { try check(notebackend_search_stop()) }
        }
    }

    public func persist() throws {
        try requireOpen {
            try transaction {
                try check(notebackend_persist())
            }
        }
    }

    public func close() {
        guard isOpen else { return }
        notebackend_stack_clear()
        notebackend_close_all()
        notebackend_stack_clear()
        isOpen = false
    }

    private func requireOpen<T>(_ body: () throws -> T) throws -> T {
        guard isOpen else { throw BackendError.closed }
        return try body()
    }

    private func transaction<T>(_ body: () throws -> T) throws -> T {
        notebackend_stack_clear()
        defer { notebackend_stack_clear() }
        return try body()
    }

    private func push(_ node: NodeID) {
        notebackend_stack_push_i32(node.backend)
        notebackend_stack_push_i32(node.id)
    }

    private func push(_ value: String) {
        let bytes = Array(value.utf8)
        bytes.withUnsafeBytes { raw in
            notebackend_stack_push_str_safe(
                raw.baseAddress?.assumingMemoryBound(to: CChar.self),
                raw.count
            )
        }
    }

    private func pushNodeList(_ nodes: [NodeID]) {
        for node in nodes.reversed() { push(node) }
        notebackend_stack_push_i32(Int32(nodes.count))
    }

    private func popNodeList() throws -> [NodeID] {
        let count = Int(try popInt())
        guard count >= 0 else { throw BackendError.malformedResponse }
        var result: [NodeID] = []
        result.reserveCapacity(count)
        for _ in 0..<count { result.append(try popNodeID()) }
        return result
    }

    private func popInt() throws -> Int32 {
        var value: Int32 = 0
        guard notebackend_stack_last_i32(&value) == 0 else {
            throw BackendError.malformedResponse
        }
        notebackend_stack_pop()
        return value
    }

    private func popNodeID() throws -> NodeID {
        let id = try popInt()
        let backend = try popInt()
        return NodeID(backend: backend, id: id)
    }

    private func popString() throws -> String {
        var pointer: UnsafePointer<UInt8>?
        var size = 0
        guard notebackend_stack_last_str(&pointer, &size) == 0,
              let pointer else { throw BackendError.malformedResponse }
        let value = String(decoding: UnsafeBufferPointer(start: pointer, count: size), as: UTF8.self)
        notebackend_stack_pop()
        return value
    }

    private func popBytes() throws -> Data {
        var pointer: UnsafePointer<UInt8>?
        var size = 0
        guard notebackend_stack_last_bytes(&pointer, &size) == 0,
              let pointer else { throw BackendError.malformedResponse }
        let value = Data(bytes: pointer, count: size)
        notebackend_stack_pop()
        return value
    }

    private func check(_ code: Int32) throws {
        guard code != 0 else { return }
        var message = "Backend operation failed (code \(code))."
        var pointer: UnsafePointer<UInt8>?
        var size = 0
        if notebackend_stack_last_str(&pointer, &size) == 0, let pointer {
            message = String(decoding: UnsafeBufferPointer(start: pointer, count: size), as: UTF8.self)
            notebackend_stack_pop()
        }
        throw BackendError.operationFailed(code: code, message: message)
    }
}
