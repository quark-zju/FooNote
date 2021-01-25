unit TreeNodeData;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, ComCtrls, NoteTypes, NoteBackend, ImageIndex, LazLogger;

type
  TTreeNodeData = class
  public
    Id: FullId;

    Mtime: NodeMtime;
    FirstLine: string;
    ChildIds: VecFullId;
    SearchText: string;

    constructor Create(AId: FullId);

    // Sync with the backend. Return true on change.
    function SyncFromBackend(): boolean;

    function ExtractMeta(Prefix: string): string;
    function GetImageIndex(): integer;
    function GetTitle(): string;
    function IsFolder(): boolean;

    // Update a key=value pair. Notify backend immediately.
    procedure SetMetaItem(Prefix, Value: string);

    // Reassign Node Id. Invalidate mtime and other data.
    procedure SetId(NewId: FullId);
  end;


function NodeData(Node: TTreeNode): TTreeNodeData;


implementation

resourcestring
  RSTrash = 'Trash';

function NodeData(Node: TTreeNode): TTreeNodeData;
begin
  assert(Node.Data <> nil);
  Result := TTreeNodeData(Node.Data);
end;

constructor TTreeNodeData.Create(AId: FullId);
begin
  Id := AId;
  Mtime := -1;
  FirstLine := '';
  SearchText := '';
  ChildIds := [];
end;

function TTreeNodeData.SyncFromBackend(): boolean;
var
  NewMtime: NodeMtime;
  url: string;
begin
  NewMtime := NoteBackend.GetMtime(Id);
  if NewMtime = Mtime then begin
    // Nothing changed.
    Exit(False);
  end;
  Mtime := NewMtime;
  // Sync Title, Meta, Children.
  FirstLine := NoteBackend.GetTextFirstLine(Id);
  ChildIds := NoteBackend.GetChildren(Id);

  // Mount on demand.
  url := ExtractMeta('mount=');
  if (not url.IsEmpty) and (not NoteBackend.IsMount(Id)) then begin
    DebugLn('Try mount %s on %s', [Url, Id.ToString()]);
    NoteBackend.TryMount(Id, Url);
  end;

  Result := True;
end;

function TTreeNodeData.ExtractMeta(Prefix: string): string;
begin
  Result := NoteBackend.ExtractMeta(Id, Prefix);
end;

function TTreeNodeData.GetImageIndex(): integer;
var
  S: string;
begin
  S := ExtractMeta('type=');
  if S = 'folder' then begin
    Result := ImageIndex.IMG_FOLDER;
  end else if S = 'root' then begin
    if NoteBackend.IsMount(Id) then begin
      Result := ImageIndex.IMG_FOLDER_LOCAL;
    end else begin
      Result := ImageIndex.IMG_FOLDER;
    end;
  end else if S = 'trash' then begin
    Result := ImageIndex.IMG_TRASH;
  end else if S = 'mount' then begin
    Result := ImageIndex.IMG_FOLDER_LOCAL;
  end else begin
    Result := ImageIndex.IMG_TEXT;
  end;
end;

function TTreeNodeData.GetTitle(): string;
var
  S: string;
begin
  Result := FirstLine;
  if Result.IsEmpty then begin
    // Check pre-defined titles.
    S := ExtractMeta('type=');
    if S = 'trash' then begin
      Result := RSTrash;
    end;
  end;
end;

procedure TTreeNodeData.SetMetaItem(Prefix, Value: string);
begin
  NoteBackend.TryUpdateMeta(Id, Prefix, Value);
end;

procedure TTreeNodeData.SetId(NewId: FullId);
begin
  if NewId = Id then begin
    exit;
  end;
  Id := NewId;
  Mtime := -1;
  FirstLine := '';
  ChildIds := [];
end;

function TTreeNodeData.IsFolder(): boolean;
begin
  Result := NoteBackend.IsFolder(Id);
end;

end.
