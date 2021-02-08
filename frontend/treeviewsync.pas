unit TreeViewSync;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, ComCtrls, TreeNodeData, NoteBackend, NoteTypes, FGL, LogFFI, Settings;

// Update a tree view node from backend. Prepare its children (for +/- sign).
procedure SyncTreeNode(View: TTreeNode; ForceChild: boolean = False);

// Update child nodes. Parent can be nil.
procedure SyncChildTreeNode(TreeView: TCustomTreeView; Parent, FirstChild: TTreeNode;
  Data: TTreeNodeData; Refresh: boolean);

// Apply data to a single tree view node.
procedure ApplyData(View: TTreeNode; Data: TTreeNodeData);


implementation

type
  TIdNodeMap = specialize TFPGMap<FullId, TTreeNode>;

procedure ApplyData(View: TTreeNode; Data: TTreeNodeData);
begin
  assert(Assigned(View) and Assigned(Data));

  if AppConfig.ShowNodeId then begin
    View.Text := Format('%s [%s]', [Data.GetTitle(), Data.Id.ToString()]);
  end else begin
    View.Text := Data.GetTitle();
  end;
  View.ImageIndex := Data.GetImageIndex();
  View.SelectedIndex := View.ImageIndex;
end;

procedure SyncTreeNode(View: TTreeNode; ForceChild: boolean = False);
var
  Data: TTreeNodeData;
  Changed: boolean;
begin
  if (View = nil) or (View.Data = nil) then begin
    exit;
  end;
  Data := TTreeNodeData(View.Data);

  // Update this node.
  Changed := Data.SyncFromBackend();
  if Changed then begin
    ApplyData(View, Data);
  end;

  // Update children.
  // If this node is not expanded, only prepare the child nodes and their
  // data, do not actually update them. This provides laziness for a large
  // backend tree.
  if Changed or ForceChild then begin
    SyncChildTreeNode(View.TreeView, View, View.GetFirstChild, Data,
      View.Expanded or ForceChild);
  end;
end;

procedure SyncChildTreeNode(TreeView: TCustomTreeView; Parent, FirstChild: TTreeNode;
  Data: TTreeNodeData; Refresh: boolean);
var
  Id: FullId;
  D: TTreeNodeData;
  ChildView, N: TTreeNode;
  IdMap: TIdNodeMap;
begin
  // Collect the existing Id -> TTreeNode mapping.
  IdMap := TIdNodeMap.Create;
  ChildView := FirstChild;
  while ChildView <> nil do begin
    D := TTreeNodeData(ChildView.Data);
    IdMap.Add(D.Id, ChildView);
    ChildView := ChildView.GetNextSibling;
  end;

  // Update TreeNode.Data (especially the id field).
  ChildView := FirstChild;
  for Id in Data.ChildIds do begin
    if ChildView = nil then begin
      // New node.
      if LogHasDebug then begin
        LogDebug(Format(' Allocating new node for %s', [Id.ToString()]));
      end;
      ChildView := TreeView.Items.AddChild(Parent, '...');
      ChildView.Data := TTreeNodeData.Create(Id);
    end else begin
      // Updating existing node.
      D := TTreeNodeData(ChildView.Data);
      if not (D.Id = Id) then begin
        if IdMap.TryGetData(Id, N) then begin
          // Move existing node N to the current position.
          if LogHasDebug then begin
            LogDebug(Format(' Moving node %s before node %s', [Id.ToString(), D.Id.ToString()]));
          end;
          N.MoveTo(ChildView, naInsert);
          ChildView := N;
          IdMap.Remove(Id);
        end else begin
          // Create a new node.
          if LogHasDebug then begin
            LogDebug(Format(' Allocating new node for %s', [Id.ToString()]));
          end;
          ChildView := TreeView.Items.Insert(ChildView, '...');
          ChildView.Data := TTreeNodeData.Create(Id);
        end;
      end;
    end;

    if Refresh then begin
      SyncTreeNode(ChildView);
    end;
    ChildView := ChildView.GetNextSibling;
  end;

  // Delete remaining nodes.
  while ChildView <> nil do begin
    N := ChildView;
    D := TTreeNodeData(ChildView.Data);
    if LogHasDebug then begin
      LogDebug(Format(' Removing node %s', [D.Id.ToString()]));
    end;
    ChildView := ChildView.GetNextSibling;
    N.Delete;
  end;

  FreeAndNil(IdMap);
end;

end.
