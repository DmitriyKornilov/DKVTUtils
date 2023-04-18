unit DK_VSTTools;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Controls, Graphics, VirtualTrees, DK_Vector, DK_Matrix,
  DK_VSTTables;

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
                       const AOnSelect: TVSTSelectEvent;
                       const ASelectedIndex: Integer = 0
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

  { TVSTCategoryDatesList }

  TVSTCategoryDatesList = class(TVSTCategoryRadioButtonTable)
  public
    constructor Create(const ATree: TVirtualStringTree;
                       const ACaption: String;
                       const AOnSelect: TVSTSelectEvent
                       );
    procedure Update(const ACategories: TStrVector;
                     const ADates: TDateMatrix;
                     const ASelectedDate: TDate = 0
                     );
  end;

implementation

{ TVSTStringList }

constructor TVSTStringList.Create(const ATree: TVirtualStringTree;
                       const ACaption: String;
                       const AItems: TStrVector;
                       const AOnSelect: TVSTSelectEvent;
                       const ASelectedIndex: Integer = 0);
begin
  inherited Create(ATree);
  SetHeaderHeight(LIST_HEADER_HEIGHT_DEFAULT);
  SetRowHeight(LIST_ROW_HEIGHT_DEFAULT);
  FTree.BorderStyle:= bsNone;
  HeaderFont.Style:= [fsBold];
  HeaderVisible:= ACaption<>EmptyStr;
  AutoHeight:= True;
  GridLinesVisible:= False;
  CanSelect:= True;
  CanUnselect:= False;
  AddColumn(ACaption, 100, taLeftJustify);
  SetColumn(ACaption, AItems, taLeftJustify);
  Draw;
  Select(ASelectedIndex);
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
  HeaderFont.Style:= [fsBold];
  AutoHeight:= True;
  GridLinesVisible:= False;
  HeaderVisible:= ACaption<>EmptyStr;
  SelectedBGColor:= FTree.Color;
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

{ TVSTCategoryDatesList }

constructor TVSTCategoryDatesList.Create(const ATree: TVirtualStringTree;
                       const ACaption: String;
                       const AOnSelect: TVSTSelectEvent
                       );
begin
  inherited Create(ATree);
  SetHeaderHeight(LIST_HEADER_HEIGHT_DEFAULT);
  SetRowHeight(LIST_ROW_HEIGHT_DEFAULT);
  FTree.BorderStyle:= bsNone;
  HeaderFont.Style:= [fsBold];
  GridLinesVisible:= False;
  HeaderVisible:= ACaption<>EmptyStr;
  SelectedBGColor:= FTree.Color;
  SelectedFont.Style:= [fsBold];
  CanUnselect:= False;
  AddColumn(ACaption);
  Draw;
  OnSelect:= AOnSelect;
end;

procedure TVSTCategoryDatesList.Update(const ACategories: TStrVector;
                     const ADates: TDateMatrix;
                     const ASelectedDate: TDate = 0);
var
  StrDates: TStrMatrix;
  i, j: Integer;
begin
  ValuesClear;
  Draw;
  if VIsNil(ACategories) or MisNil(ADates) then Exit;

  SetCategories(ACategories);
  StrDates:= MFormatDateTime('dd.mm.yyyy', ADates);
  SetColumn(0, StrDates, taLeftJustify);
  Draw;


  i:= 0;
  j:= 0;
  if ASelectedDate>0 then
    MIndexOfDate(ADates, ASelectedDate, i, j);
  if i<0 then i:= 0;
  if j<0 then j:= 0;
  Select(i, j);
  Show(i, j);
end;

end.

