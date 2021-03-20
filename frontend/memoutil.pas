unit MemoUtil;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, StdCtrls, LazUTF8;


// Handle TAB, ENTER in a smarter way.
procedure SmartKeyDown(Memo: TMemo; var Key: word; Shift: TShiftState);

implementation

procedure SmartKeyDown(Memo: TMemo; var Key: word; Shift: TShiftState);
var
  Y: longint;
  Line: string;
  SpaceCount: integer;
  Prefix: string;
begin
  // X, Y, SelLen: UTF8 Code Points.
  // SelLen uses CRLF on Windows as EOL.
  Y := Memo.CaretPos.Y;

  // Auto indent: try to insert spaces from the previous line.
  // "  - foo <ENTER>" will insert "  " in the next line.
  // KeyPress runs before the side effect of Key.
  if (key = 13) or (key = 10) and (Memo.SelLength = 0) then begin
    Line := Memo.Lines[Y];
    // At the end of the line.
    if LazUTF8.UTF8Length(Line) = Memo.CaretPos.X then begin
      SpaceCount := 0;
      while SpaceCount < Line.Length do begin
        if Line.Chars[SpaceCount] <> ' ' then break;
        SpaceCount += 1;
      end;
      if SpaceCount > 0 then begin
        Prefix := StringOfChar(' ', SpaceCount);
        Memo.Lines.Insert(Y + 1, Prefix);
        Memo.CaretPos := TPoint.Create(SpaceCount, Y + 1);
        key := 0;
      end;
    end;
  end;
end;


end.
