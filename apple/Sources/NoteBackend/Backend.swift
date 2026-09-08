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
            try transaction {
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
    public func insert(parent: NodeID, text: String) throws -> NodeID {
        try requireOpen {
            try transaction {
                // InsertPos::Append is encoded as 0 by the Rust ABI.
                push(parent)
                notebackend_stack_push_i32(0)
                push(text)
                push("")
                try check(notebackend_insert())
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
