unit MemoUtil;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, StdCtrls, LazUTF8;


// Handle TAB, ENTER in a smarter way.
// - Tab, Shift+Tab: Indent or Dedent if multiple lines are selected (at least one #10).
// - Enter: Copy space prefix from the previous line.
procedure SmartKeyDown(Memo: TMemo; var Key: word; Shift: TShiftState);


implementation

{
  TMemo:

    TMemo.SelStart: Count by "logical" characters (ex. '我' counts as 1), not by bytes.
    TMemo.Lines: *Affected* by line wraps (so, not a great source to operate on).
    TMemo.Text: OS-EOL (ex. \r\n #13#10 on Windows).

  string and pos:

    S:='abc';
    S[1] // a
    S[2] // b
    Length(S) // 3
    Pos('b', S) // 2
    Pos('b', S, 1) // 2
    Pos('b', S, 2) // 2
    Pos('b', S, 3) // 0
    Copy(S, 2, 1) // b

    S:='字abc';
    Pos('b', S) // 5
    Length(S) // 6, byte size
    S[5] // b
}

type
  TEditLineFunc = function(S: string): string;

// Return the next line (including CRLF or LF) from S.
// StartPos initially should be 0.
// Return an empty string if there are no more lines.
function NextLine(const S: string; var StartPos: longint): string;
var
  P, LineLen: longint;
begin
  P := Pos(#10, S, StartPos + 1);
  if P = 0 then begin
    LineLen := Length(S) - StartPos;
  end else begin
    LineLen := P {starts from 1} - StartPos {starts from 0};
  end;
  if LineLen > 0 then begin
    Result := Copy(S, StartPos + 1, LineLen);
    StartPos += LineLen;
  end else begin
    Result := '';
  end;
end;

// Edit lines in YStart..=YEndInclusive range using EditFunc.
function EditLines(S: string; YStart, YEndInclusive: longint; EditFunc: TEditLineFunc): string;
var
  Line: string;
  LineIndex, StartPos: longint;
  ResultList: TStringList;
begin
  ResultList := TStringList.Create;
  ResultList.LineBreak := '';
  StartPos := 0;
  LineIndex := 0;
  while True do begin
    Line := NextLine(S, StartPos);
    if Line.IsEmpty then begin
      break;
    end;
    if (LineIndex >= YStart) and (LineIndex <= YEndInclusive) then begin
      Line := EditFunc(Line);
    end;
    ResultList.Append(Line);
    LineIndex += 1;
  end;
  Result := ResultList.Text;
  FreeAndNil(ResultList);
end;

// Edit lines in a TMemo.
procedure EditMemoLines(AMemo: TMemo; YStart, YEndInclusive: longint; EditFunc: TEditLineFunc);
var
  S: string;
begin
  S := AMemo.Text;
  S := EditLines(S, YStart, YEndInclusive, EditFunc);
  AMemo.Text := S;
end;

// Dedent a line.
function Dedent(S: string): string;
begin
  if S.StartsWith(#9) then begin
    Result := S.Substring(1);
  end else if S.StartsWith('  ') then begin
    Result := S.Substring(2);
  end else begin
    Result := S;
  end;
end;

// Indent a line.
function Indent(S: string): string;
begin
  if S.IsEmpty or S.Trim.IsEmpty then begin
    Result := S;
  end else begin
    Result := #9 + S;
  end;
end;

// Find the range of Y covering selected text. Both YStart and YEnd are inclusive.
procedure GetMemoSelectedLines(Memo: TMemo; var YStart: longint; var YEnd: longint);
var
  Text: string;
begin
  Text := Memo.Text;
  YStart := LazUtf8.UTF8LeftStr(Text, Memo.SelStart).CountChar(#10);
  YEnd := YStart + Memo.SelText.CountChar(#10);
  if YStart = YEnd then begin
    // Do not treat it as line selection. YStart will be > YEnd in this case.
    YEnd -= 1;
  end else if (YEnd > YStart) and Memo.SelText.EndsWith(#10) then begin
    // Do not treat the beginning of a new line as "selected".
    YEnd -= 1;
  end;
end;

// Select YStart..=YEnd *full* lines (including end of line) in a TMemo.
procedure SetMemoSelectedLines(AMemo: TMemo; YStart: longint; YEnd: longint);
var
  S, Line: string;
  LineIndex, {EolLen,} LeftLen, StartPos, SelUtf8Start, SelUtf8Len: longint;
begin
  S := AMemo.Text;
  StartPos := 0;
  LineIndex := 0;
  SelUtf8Len := 0;
  while True do begin
    LeftLen := StartPos;
    Line := NextLine(S, StartPos);
    if Line.IsEmpty then begin
      break;
    end;
    if LineIndex = YStart then begin
      SelUtf8Start := LazUtf8.UTF8Length(S.Substring(0, LeftLen));
    end;
    if (LineIndex >= YStart) and (LineIndex <= YEnd) then begin
      SelUtf8Len += LazUtf8.UTF8Length(Line);
    end;
    LineIndex += 1;
  end;
  AMemo.SelStart := SelUtf8Start;
  // Remove the last '\n'. Better preserve it.
  // EolLen := LazUtf8.UTF8Length(AMemo.Lines.LineBreak);
  // if SelUtf8Len >= EolLen then begin
  //   SelUtf8Len -= EolLen;
  // end;
  AMemo.SelLength := SelUtf8Len;
end;

// Get the index-th (starts from 0) line including ending (ex. CRLF or LF).
function GetMemoLine(S: string; Index: integer): string;
var
  Line: string;
  LineIndex, StartPos: longint;
begin
  LineIndex := 0;
  StartPos := 0;
  while True do begin
    Line := NextLine(S, StartPos);
    if Line.IsEmpty then begin
      break;
    end;
    if LineIndex = Index then begin
      Result := Line;
      break;
    end;
    LineIndex += 1;
  end;
end;


procedure SmartKeyDown(Memo: TMemo; var Key: word; Shift: TShiftState);
var
  YStart, YEnd: longint;
  Line: string;
  SpaceCount: integer;
begin
  YStart := 0;
  YEnd := 0;

  // Auto indent: try to insert spaces from the previous line.
  // "  - foo <ENTER>" will insert "  " in the next line.
  // KeyPress runs before the side effect of Key.
  if ((key = 13) or (key = 10)) and (Memo.SelLength = 0) then begin
    GetMemoSelectedLines(Memo, YStart, YEnd);
    // This is the previous line after inserting the new line.
    // Currently the new line is not inserted yet.
    Line := GetMemoLine(Memo.Text, YStart);
    SpaceCount := 0;
    // Count starting TABs or SPACEs.
    while SpaceCount < Line.Length do begin
      if (Line.Chars[SpaceCount] <> #9) and (Line.Chars[SpaceCount] <> ' ') then begin
        break;
      end;
      SpaceCount += 1;
    end;
    // Insert the spaces into the new line.
    if SpaceCount > 0 then begin
      Memo.SelText := Memo.Lines.LineBreak + Line.Substring(0, SpaceCount);
      key := 0;
    end;
  end;

  // Smart TAB: Indent or dedent selected lines.
  if key = 9 then begin
    if ssShift in Shift then begin
      // De-dent selected lines.
      GetMemoSelectedLines(Memo, YStart, YEnd);
      if YEnd >= YStart then begin
        EditMemoLines(Memo, YStart, YEnd, @Dedent);
        SetMemoSelectedLines(Memo, YStart, YEnd);
        Key := 0;
      end;
    end else begin
      if Memo.SelLength = 0 then begin
        // No selection. Insert a TAB.
        // Memo.SelText := #9;
      end else begin
        // With selection. Indent lines.
        GetMemoSelectedLines(Memo, YStart, YEnd);
        if YEnd >= YStart then begin
          EditMemoLines(Memo, YStart, YEnd, @Indent);
          SetMemoSelectedLines(Memo, YStart, YEnd);
          Key := 0;
        end;
      end;
    end;
  end;
end;


end.
