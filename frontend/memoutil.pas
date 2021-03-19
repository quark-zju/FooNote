unit MemoUtil;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, StdCtrls, LazUTF8;


// Find the range of Y covering selected text.
procedure SelectedYRange(Memo: TMemo; var YStart: longint; var YEnd: longint);

// Select full lines in a range.
procedure SelectLines(Memo: TMemo; YStart: longint; YEnd: longint);

// Dedent a line.
procedure DedentLine(Memo: TMemo; Y: longint);

// Indent a line.
procedure IndentLine(Memo: TMemo; Y: longint);

// Handle TAB, ENTER in a smarter way.
procedure SmartKeyDown(Memo: TMemo; var Key: word; Shift: TShiftState);

implementation

procedure DedentLine(Memo: TMemo; Y: longint);
var
  Line: string;
begin
  Line := Memo.Lines[Y];
  if Line.StartsWith('  ') then begin
    Line := Line.Substring(2);
    Memo.Lines[Y] := Line;
  end;
end;

procedure IndentLine(Memo: TMemo; Y: longint);
var
  Line: string;
begin
  Line := Memo.Lines[Y];
  if not Line.IsEmpty then begin
    Line := Line.Insert(0, '  ');
    Memo.Lines[Y] := Line;
  end;
end;


// Find the range of Y covering selected text.
procedure SelectedYRange(Memo: TMemo; var YStart: longint; var YEnd: longint);
var
  Text: string;
begin
  if Memo.SelLength = 0 then begin
    YStart := Memo.CaretPos.Y;
    YEnd := YStart;
  end;
  // Is there a faster way? Memo.Lines? Is it separated as CRLF or LF?
  Text := Memo.Text;
  YStart := LazUtf8.UTF8LeftStr(Text, Memo.SelStart).CountChar(#10);
  YEnd := YStart + Memo.SelText.CountChar(#10);
end;

procedure SelectLines(Memo: TMemo; YStart: longint; YEnd: longint);
var
  I, Len, EolLen: longint;
begin
  Memo.CaretPos := TPoint.Create(0, YStart);
  Len := 0;
  EolLen := LazUtf8.UTF8Length(Memo.Lines.LineBreak);
  for I := YStart to YEnd do begin
    Len += LazUtf8.UTF8Length(Memo.Lines[I]);
    if I < YEnd then Len += EolLen;
  end;
  Memo.SelLength := Len;
end;

procedure SmartKeyDown(Memo: TMemo; var Key: word; Shift: TShiftState);
var
  X, Y, YStart, YEnd: longint;
  Line: string;
  SpaceCount: integer;
  Prefix: string;
begin
  // X, Y, SelLen: UTF8 Code Points.
  // SelLen uses CRLF on Windows as EOL.
  X := Memo.CaretPos.X;
  Y := Memo.CaretPos.Y;
  YStart := 0;
  YEnd := 0;

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

  // Smart TAB: Indent or dedent selected lines.
  if key = 9 then begin
    if ssShift in Shift then begin
      // De-dent selected lines.
      SelectedYRange(Memo, YStart, YEnd);
      for Y := YStart to YEnd do DedentLine(Memo, Y);
      SelectLines(Memo, YStart, YEnd);
    end else begin
      if Memo.SelLength = 0 then begin
        // No selection. Insert 2 spaces.
        Memo.SelText:='  ';
      end else begin
        // With selection. Indent lines.
        SelectedYRange(Memo, YStart, YEnd);
        for Y := YStart to YEnd do IndentLine(Memo, Y);
        SelectLines(Memo, YStart, YEnd);
      end;
    end;
    key := 0;
  end;
end;


end.
