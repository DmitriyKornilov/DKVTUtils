unit DK_VSTTableTools;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Controls, Graphics, VirtualTrees,

  DK_Vector, DK_Matrix, DK_StrUtils, DK_VSTTables, DK_VSTCategoryTables,
  DK_VSTTypes;

const
  TOOLS_ROW_HEIGHT_DEFAULT = 18;

type

  { TVSTStringList }

  TVSTStringList = class(TVSTTable)
  private
    function GetItemIndex: Integer;
    procedure SetItemIndex(const AValue: Integer);
  public
    constructor Create(const ATree: TVirtualStringTree;
                       const ACaption: String;
                       const AOnSelect: TVSTEvent
                       );
    procedure Update(const AItems: TStrVector; const ASelectedIndex: Integer = 0);
    property ItemIndex: Integer read GetItemIndex write SetItemIndex;
  end;

  { TVSTCheckList }

  TVSTCheckList = class(TVSTCheckTable)
  public
    constructor Create(const ATree: TVirtualStringTree;
                       const ACaption: String;
                       const AOnSelect: TVSTEvent);
    procedure Update(const AItems: TStrVector;
                     const ACheckeds: TBoolVector = nil);
  end;

  { TVSTCategoryList }

  TVSTCategoryList = class(TVSTCategoryRadioTable)
  protected
    procedure DoUpdate(const ACategories: TStrVector;
                       const AValues: TStrMatrix;
                       const ASelectedIndex1, ASelectedIndex2: Integer
                       );
  public
    constructor Create(const ATree: TVirtualStringTree;
                       const ACaption: String;
                       const AOnSelect: TVSTEvent
                       );
  end;

  { TVSTCategoryDateList }

  TVSTCategoryDateList = class(TVSTCategoryList)
  public
    procedure Update(const ACategories: TStrVector;
                     const ADates: TDateMatrix;
                     const ASelectedDate: TDate = 0
                     );
  end;

  { TVSTCategoryIDList }

  TVSTCategoryIDList = class(TVSTCategoryList)
  public
    procedure Update(const ACategories: TStrVector;
                     const AValues: TStrMatrix;
                     const AIDs: TIntMatrix;
                     const ASelectedID: Integer = -1
                     );
  end;

implementation

procedure SetSimpleListParams(const AList: TVSTCoreTable; const ACaption: String);
begin
  AList.RowHeight:= AList.Tree.Scale96ToScreen(TOOLS_ROW_HEIGHT_DEFAULT);
  AList.Tree.BorderStyle:= bsNone;
  AList.HeaderFont.Style:= [fsBold];
  AList.HeaderVisible:= not SEmpty(ACaption);
  AList.GridLinesVisible:= False;
  AList.AutoHeight:= True;
  AList.AddColumn(ACaption, 100, taLeftJustify);
end;

{ TVSTStringList }

function TVSTStringList.GetItemIndex: Integer;
begin
  Result:= -1;
  if not IsSelected then Exit;
  Result:= SelectedIndex;
end;

procedure TVSTStringList.SetItemIndex(const AValue: Integer);
begin
  if IsIndexCorrect(AValue) then
    Select(AValue);
end;

constructor TVSTStringList.Create(const ATree: TVirtualStringTree;
                       const ACaption: String;
                       const AOnSelect: TVSTEvent
                       );
begin
  inherited Create(ATree);
  SetSimpleListParams(Self, ACaption);
  CanSelect:= True;
  CanUnselect:= False;
  Draw;
  OnSelect:= AOnSelect;
end;

procedure TVSTStringList.Update(const AItems: TStrVector; const ASelectedIndex: Integer = 0);
begin
  FTree.Visible:= False;
  try
    ValuesClear;
    SetColumn(0, AItems, taLeftJustify);
    Draw;
    if not VIsNil(AItems) then
    begin
      if IsIndexCorrect(ASelectedIndex) then
        Select(ASelectedIndex)
      else
        Select(0);
    end;
  finally
    FTree.Visible:= True;
  end;
end;

{ TVSTCheckList }

constructor TVSTCheckList.Create(const ATree: TVirtualStringTree;
                                 const ACaption: String;
                                 const AOnSelect: TVSTEvent);
begin
  inherited Create(ATree);
  SetSimpleListParams(Self, ACaption);
  SelectedBGColor:= FTree.Color;
  Draw;
  OnSelect:= AOnSelect;
end;

procedure TVSTCheckList.Update(const AItems: TStrVector;
                     const ACheckeds: TBoolVector = nil);
begin
  FTree.Visible:= False;
  try
    ValuesClear;
    SetColumn(0, AItems, taLeftJustify);
    Draw;
    if (not VIsNil(AItems)) and (not VIsNil(ACheckeds)) then
      Checkeds:= ACheckeds;
  finally
    FTree.Visible:= True;
  end;
end;

{ TVSTCategoryList }

procedure TVSTCategoryList.DoUpdate(const ACategories: TStrVector;
                       const AValues: TStrMatrix;
                       const ASelectedIndex1, ASelectedIndex2: Integer);
begin
  FTree.Visible:= False;
  try
    SetCategories(ACategories);
    SetColumn(0, AValues, taLeftJustify);
    Draw;
    if (ASelectedIndex1<0) and (ASelectedIndex2<0) then Exit;
    Select(ASelectedIndex1, ASelectedIndex2);
    Show(ASelectedIndex1, ASelectedIndex2);
  finally
    FTree.Visible:= True;
  end;
end;

constructor TVSTCategoryList.Create(const ATree: TVirtualStringTree;
                       const ACaption: String;
                       const AOnSelect: TVSTEvent);
begin
  inherited Create(ATree);
  FTree.BorderStyle:= bsNone;
  HeaderFont.Style:= [fsBold];
  GridLinesVisible:= False;
  HeaderVisible:= not SEmpty(ACaption);
  SelectedBGColor:= FTree.Color;
  SelectedFont.Style:= [fsBold];
  CanUnselect:= False;
  AddColumn(ACaption);
  Draw;
  OnSelect:= AOnSelect;
end;

{ TVSTCategoryDateList }

procedure TVSTCategoryDateList.Update(const ACategories: TStrVector;
                     const ADates: TDateMatrix;
                     const ASelectedDate: TDate = 0);
var
  StrDates: TStrMatrix;
  i, j: Integer;
begin
  ValuesClear;
  Draw;
  if VIsNil(ACategories) or MIsNil(ADates) then Exit;

  i:= -1;
  j:= -1;
  if ASelectedDate>0 then
    MIndexOfDate(ADates, ASelectedDate, i, j);
  if i<0 then i:= 0;
  if j<0 then j:= 0;

  StrDates:= MFormatDateTime('dd.mm.yyyy', ADates);
  DoUpdate(ACategories, StrDates, i, j);
end;

{ TVSTCategoryIDList }

procedure TVSTCategoryIDList.Update(const ACategories: TStrVector;
                     const AValues: TStrMatrix;
                     const AIDs: TIntMatrix;
                     const ASelectedID: Integer = -1);
var
  i, j: Integer;
begin
  ValuesClear;
  Draw;
  if VIsNil(ACategories) or MIsNil(AValues) then Exit;

  i:= -1;
  j:= -1;
  if ASelectedID>=0 then
    MIndexOf(AIDs, ASelectedID, i, j);
  if i<0 then i:= 0;
  if j<0 then j:= 0;

  DoUpdate(ACategories, AValues, i, j);
end;

end.

