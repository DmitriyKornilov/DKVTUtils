unit DK_VSTTools;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Controls, StdCtrls, Graphics, VirtualTrees,

  DK_Vector, DK_Matrix, DK_StrUtils, DK_VSTTables, DK_PPI;

const
  TOOLS_ROW_HEIGHT_DEFAULT = 18;

type

  { TVSTColorList }

  TVSTColorList = class(TVSTEdit)
  private
    procedure NodeClick(Sender: TBaseVirtualTree; const HitInfo: THitInfo);
  public
    constructor Create(const ATree: TVirtualStringTree);
    procedure Update(const AItems: TStrVector; const AColors: TColorVector);
  end;

  { TVSTStringList }

  TVSTStringList = class(TVSTTable)
  public
    constructor Create(const ATree: TVirtualStringTree;
                       const ACaption: String;
                       const AOnSelect: TVSTSelectEvent
                       );
    procedure Update(const AItems: TStrVector; const ASelectedIndex: Integer = 0);
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

  { TVSTCategoryList }

  TVSTCategoryList = class(TVSTCategoryRadioButtonTable)
  protected
    procedure DoUpdate(const ACategories: TStrVector;
                       const AValues: TStrMatrix;
                       const ASelectedIndex1, ASelectedIndex2: Integer
                       );
  public
    constructor Create(const ATree: TVirtualStringTree;
                       const ACaption: String;
                       const AOnSelect: TVSTSelectEvent
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

{ TVSTColorList }

procedure TVSTColorList.NodeClick(Sender: TBaseVirtualTree; const HitInfo: THitInfo);
begin
  Select(HitInfo.HitNode^.Index, 0);
end;

constructor TVSTColorList.Create(const ATree: TVirtualStringTree);
begin
  inherited Create(ATree);
  FTree.ShowHint:= False;
  FTree.BorderStyle:= bsNone;
  ColorColumnBorderColor:= clBlack;
  ColorColumnCellMargin:= 3;
  HeaderVisible:= False;
  GridLinesVisible:= False;
  CanSelect:= True;
  CanUnselect:= True;
  AddColumnColor('Цвет', 30);
  AddColumnRowTitles('Наименование');
  AutosizeColumnEnable('Наименование');
  Draw;
  FTree.OnNodeClick:= @NodeClick;
end;

procedure TVSTColorList.Update(const AItems: TStrVector; const AColors: TColorVector);
begin
  SetColumnRowTitles(AItems, taLeftJustify);
  SetColumnColor('Цвет', AColors);
  Draw;
end;

{ TVSTStringList }

constructor TVSTStringList.Create(const ATree: TVirtualStringTree;
                       const ACaption: String;
                       const AOnSelect: TVSTSelectEvent
                       );
begin
  inherited Create(ATree);
  SetRowHeight(HeightFromDefaultToScreen(TOOLS_ROW_HEIGHT_DEFAULT));
  FTree.BorderStyle:= bsNone;
  HeaderFont.Style:= [fsBold];
  HeaderVisible:= not SEmpty(ACaption);
  AutoHeight:= True;
  GridLinesVisible:= False;
  CanSelect:= True;
  CanUnselect:= False;
  AddColumn(ACaption, 100, taLeftJustify);
  Draw;
  OnSelect:= AOnSelect;
end;

procedure TVSTStringList.Update(const AItems: TStrVector; const ASelectedIndex: Integer = 0);
begin
  SetColumn(0, AItems, taLeftJustify);
  Draw;
  if VIsNil(AItems) then
    Unselect
  else if IsIndexCorrect(ASelectedIndex) then
    Select(ASelectedIndex)
  else
    Select(0);
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
  SetRowHeight(HeightFromDefaultToScreen(TOOLS_ROW_HEIGHT_DEFAULT));
  FTree.BorderStyle:= bsNone;
  HeaderFont.Style:= [fsBold];
  AutoHeight:= True;
  GridLinesVisible:= False;
  HeaderVisible:= not SEmpty(ACaption);
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

{ TVSTCategoryList }

procedure TVSTCategoryList.DoUpdate(const ACategories: TStrVector;
                       const AValues: TStrMatrix;
                       const ASelectedIndex1, ASelectedIndex2: Integer);
begin
  SetCategories(ACategories);
  SetColumn(0, AValues, taLeftJustify);
  Draw;
  if (ASelectedIndex1<0) and (ASelectedIndex2<0) then Exit;
  Select(ASelectedIndex1, ASelectedIndex2);
  Show(ASelectedIndex1, ASelectedIndex2);
end;

constructor TVSTCategoryList.Create(const ATree: TVirtualStringTree;
                       const ACaption: String;
                       const AOnSelect: TVSTSelectEvent);
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

