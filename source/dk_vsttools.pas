unit DK_VSTTools;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Controls, Graphics, VirtualTrees, DK_Vector, DK_VSTTables;

const
  LIST_ROW_HEIGHT_DEFAULT = 20;
  LIST_HEADER_HEIGHT_DEFAULT = 26;

type

  { TVSTStringList }

  TVSTStringList = class(TVSTTable)
  public
    constructor Create(const ATree: TVirtualStringTree;
                       const ACaption: String;
                       const AItems: TStrVector;
                       const AOnSelect: TVSTSelectEvent
                       );
  end;

  { TVSTCheckList }

  TVSTCheckList = class(TVSTCheckTable)
  public
    constructor Create(const ATree: TVirtualStringTree;
                       const ACaption: String;
                       const AItems: TStrVector;
                       const AOnSelect: TVSTSelectEvent;
                       const ACheckedCount: Integer = -1 //-1 check all, >=0 check [0..ACheckedCount-1]
                       );
  end;

implementation

{ TVSTStringList }

constructor TVSTStringList.Create(const ATree: TVirtualStringTree;
                       const ACaption: String;
                       const AItems: TStrVector;
                       const AOnSelect: TVSTSelectEvent);
begin
  inherited Create(ATree);
  SetHeaderHeight(LIST_HEADER_HEIGHT_DEFAULT);
  SetRowHeight(LIST_ROW_HEIGHT_DEFAULT);
  FTree.BorderStyle:= bsNone;
  HeaderFont.Style:= HeaderFont.Style + [fsBold];
  HeaderVisible:= ACaption<>EmptyStr;
  AutoHeight:= True;
  GridLinesVisible:= False;
  CanSelect:= True;
  CanUnselect:= False;
  AddColumn(ACaption, 100, taLeftJustify);
  SetColumn(ACaption, AItems, taLeftJustify);
  Draw;
  Select(0);
  OnSelect:= AOnSelect;
end;

{ TVSTCheckList }

constructor TVSTCheckList.Create(const ATree: TVirtualStringTree;
     const ACaption: String;
     const AItems: TStrVector;
     const AOnSelect: TVSTSelectEvent;
     const ACheckedCount: Integer = -1 //-1 check all, >=0 check [0..ACheckedCount-1]
     );
var
  i: Integer;
begin
  inherited Create(ATree);
  SetHeaderHeight(LIST_HEADER_HEIGHT_DEFAULT);
  SetRowHeight(LIST_ROW_HEIGHT_DEFAULT);
  FTree.BorderStyle:= bsNone;
  HeaderFont.Style:= HeaderFont.Style + [fsBold];
  AutoHeight:= True;
  GridLinesVisible:= False;
  HeaderVisible:= ACaption<>EmptyStr;
  SelectedBGColor:= COLOR_BG_DEFAULT;
  AddColumn(ACaption, 100, taLeftJustify);
  SetColumn(ACaption, AItems, taLeftJustify);
  Draw;

  if ACheckedCount=-1 then
    CheckAll(True)
  else if (ACheckedCount>=0) and (ACheckedCount<=Length(AItems)) then
    for i:=0 to ACheckedCount-1 do
      Check(i);

  OnSelect:= AOnSelect;
end;

end.

