unit saphHistory;

{$mode objfpc}{$H+}

interface

uses
    Classes, SysUtils, fpcunit, testutils, testregistry, saph.reactive;

type
    TStringHistory = class(specialize GUndoHistory<string>);

	{ TTestSaphHistory }

    TTestSaphHistory= class(TTestCase)
    protected
        procedure SetUp; override;
        procedure TearDown; override;
    published
        procedure testNextHistPos;
        procedure testHistoryCount;
        procedure testUndoRedo;
        procedure testUndoRedoBoundaries;
    end;

implementation
uses
    math;

procedure TTestSaphHistory.testNextHistPos;
var
	i, a: Integer;
    strHist: TStringHistory;
begin
    strHist := TStringHistory.Create;
    try
	    {Checks the logic by which next head position is calculated}
	    for i := 0 to pred(strHist.UPPER_LIM) do begin
            a := strHist.Add(IntToStr(getTickCount64()));
	        AssertTrue(Format('FAIL 1): a:%d; i:%d', [a, i]), (a = i));
	    end;

	    // Reset and cycle
	    for i := strHist.UNDOSIZE to pred(strHist.UPPER_LIM) do begin
            a := strHist.Add(IntToStr(getTickCount64()));
	        AssertTrue(Format('FAIL 2): a:%d; i:%d', [a, i]), (a = i));
	    end;

	    // Reset and cycle
	    for i := strHist.UNDOSIZE to pred(strHist.UPPER_LIM) do begin
	        a := strHist.Add(IntToStr(getTickCount64()));
	        AssertTrue(Format('FAIL 3): a:%d; i:%d', [a, i]), (a = i));
	    end;

	    // Reset and cycle
	    for i := strHist.UNDOSIZE to pred(strHist.UPPER_LIM) do begin
            a := strHist.Add(IntToStr(getTickCount64()));
	        AssertTrue(Format('FAIL 4): a:%d; i:%d', [a, i]), (a = i));
	    end;
	finally
	    strHist.Free;
    end;
end;

procedure TTestSaphHistory.testHistoryCount;
var
	i: Integer;
    strHist: TStringHistory;
begin
    strHist := TStringHistory.Create;
    try
        {ZERO History}
        AssertTrue(Format('FAIL 1: strHist.histCount :%d; i: %d  ', [strHist.histCount, 0]), (strHist.histCount = 0));

	    {Checks the logic of the history count}
	    for i := 1 to strHist.UNDOSIZE do begin
            strHist.Add(IntToStr(getTickCount64()));
	        AssertTrue(Format('FAIL 1: strHist.histCount :%d; i: %d  ', [strHist.histCount, i]), (strHist.histCount = i));
	    end;

        // Once the number of items is more than undo size, the
        // history count should remain fixed at UNDOSIZE
        for i := 0 to pred(strHist.UNDOSIZE) do begin
            strHist.Add(IntToStr(getTickCount64()));
	        AssertTrue(Format('FAIL 2: strHist.histCount :%d; i: %d  ', [strHist.histCount, i]), (strHist.histCount = strHist.UNDOSIZE));
	    end;

	finally
	    strHist.Free;
    end;
end;

procedure TTestSaphHistory.testUndoRedo;
var
    _strs: array[0..9] of string = (
            'ONE',
            'TWO',
            'THREE',
            'FOUR',
            'FIVE',
            'SIX',
            'SEVEN',
            'EIGHT',
            'NINE',
            'TEN'
    );

	i: Integer;
    strHist: TStringHistory;

begin
    strHist := TStringHistory.Create;
    try
        {Checks the logic of the history}
	    for i := 0 to pred(Length(_strs)) do begin
            strHist.Add(_strs[i]);
	        AssertTrue(Format('FAIL 1: i: %d; str="%s"',[i, _strs[i]]), (strHist.currVal = _strs[i]));
        end;
        i := pred(Length(_strs));

        {UNDO}
        strHist.undo;
        dec(i);
        AssertTrue(Format('FAIL 2 i: %d; currval: "%s",  str="%s"',[i, strHist.currVal, _strs[i]]), (strHist.currVal = _strs[i]));

        strHist.undo;
        dec(i);
        AssertTrue(Format('FAIL 2: i: %d; currval: "%s",  str="%s"',[i, strHist.currVal, _strs[i]]), (strHist.currVal = _strs[i]));

        strHist.undo;
        dec(i);
        AssertTrue(Format('FAIL 2: i: %d; currval: "%s",  str="%s"',[i, strHist.currVal, _strs[i]]), (strHist.currVal = _strs[i]));

        strHist.undo;
        dec(i);
        AssertTrue(Format('FAIL 2: i: %d; currval: "%s",  str="%s"',[i, strHist.currVal, _strs[i]]), (strHist.currVal = _strs[i]));

        {REDO}
        strHist.redo;
        inc(i);
        AssertTrue(Format('FAIL 3: i: %d; currval: "%s",  str="%s"',[i, strHist.currVal, _strs[i]]), (strHist.currVal = _strs[i]));

        strHist.redo;
        inc(i);
        AssertTrue(Format('FAIL 3: i: %d; currval: "%s",  str="%s"',[i, strHist.currVal, _strs[i]]), (strHist.currVal = _strs[i]));

        strHist.redo;
        inc(i);
        AssertTrue(Format('FAIL 3: i: %d; currval: "%s",  str="%s"',[i, strHist.currVal, _strs[i]]), (strHist.currVal = _strs[i]));

        strHist.redo;
        inc(i);
        AssertTrue(Format('FAIL 3: i: %d; currval: "%s",  str="%s"',[i, strHist.currVal, _strs[i]]), (strHist.currVal = _strs[i]));

	finally
	    strHist.Free;
    end;
end;

procedure TTestSaphHistory.testUndoRedoBoundaries;
var
    _strs: array[0..9] of string = (
            'ONE',
            'TWO',
            'THREE',
            'FOUR',
            'FIVE',
            'SIX',
            'SEVEN',
            'EIGHT',
            'NINE',
            'TEN'
    );

	i, h, p, j, c: Integer;
    strHist: TStringHistory;

begin
    strHist := TStringHistory.Create;
    try
        {Checks the logic of the history}
	    for i := 0 to pred(Length(_strs)) do begin
            strHist.Add(_strs[i]);
	        AssertTrue(Format('FAIL 1: i: %d; str="%s"',[i, _strs[i]]), (strHist.currVal = _strs[i]));
        end;
        i := pred(Length(_strs));

        {UNDO}
        h := strHist.currHead;
        p := strHist.currPos;

        i := 20;
        strHist.undo(i);

        c := strHist.currPos; // after undo;
        AssertTrue(Format('FAIL 2: p: %d; c=%d',[p, c]), (p>=c)); // current pos has moved;

        j := max(0, h - pred(strHist.histCount));
        AssertTrue(Format('FAIL 3: c: %d; j=%d',[c, j]), (c = j)); // check if the movement of current pos after undo is correct

        AssertTrue(Format('FAIL 4: currVal: "%s"; _strs[%d]: "%s"',[strHist.currVal, j, _strs[j]]), (strHist.currVal= _strs[j])); // current pos has moved;

        strHist.redo(i);
        AssertTrue(Format('FAIL 5 REDO: currVal: "%s"; _strs[%d]: "%s"',[strHist.currVal, j, _strs[9]]), (strHist.currVal= _strs[9])); // current pos has moved;


        strHist.undo(i);
        strHist.add('CC'); inc(j);
        strHist.add('BB'); inc(j);
        strHist.add('AA'); inc(j);

        AssertTrue(Format('FAIL 6 New history: currPos :%d; j: %d"',[strHist.currPos, j]), (strHist.currPos= j));

        strHist.undo(2); dec(j, 2);
        AssertTrue(Format('FAIL 6 New history: currPos :"%s"; j: "%s"',[strHist.currVal, 'CC']), (strHist.currVal = 'CC'));

        strHist.add('new history'); inc(j);
        strHist.redo;
        AssertFalse(Format('FAIL 7 Redo: currPos :"%s"; j: "%s"',[strHist.currVal, 'AA']), (strHist.currVal = 'AA'));

	finally
	    strHist.Free;
    end;
end;


procedure TTestSaphHistory.SetUp;
begin

end;

procedure TTestSaphHistory.TearDown;
begin

end;

initialization

    RegisterTest(TTestSaphHistory);
end.

