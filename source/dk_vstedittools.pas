unit DK_VSTEditTools;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Graphics, Controls, VirtualTrees,
  DK_VSTTypes, DK_VSTEdit, DK_Vector, DK_Color;

type

  { TVSTColorList }

  TVSTColorList = class(TVSTEdit)
  protected
    procedure BeforeCellPaint(Sender: TBaseVirtualTree; TargetCanvas: TCanvas;
                              Node: PVirtualNode; Column: TColumnIndex;
                              CellPaintMode: TVTCellPaintMode; CellRect: TRect;
                              var {%H-}ContentRect: TRect); override;
    function CellBGColor(Node: PVirtualNode; {%H-}Column: TColumnIndex): TColor; override;
    procedure NodeClick(Sender: TBaseVirtualTree; const HitInfo: THitInfo);
    function GetColors: TColorVector;
  public
    constructor Create(const ATree: TVirtualStringTree);
    procedure Update(const AItems: TStrVector; const AColors: TColorVector);
    property Colors: TColorVector read GetColors;
  end;

implementation

{ TVSTColorList }

procedure TVSTColorList.NodeClick(Sender: TBaseVirtualTree; const HitInfo: THitInfo);
begin
  Select(HitInfo.HitNode^.Index, 0);
end;

function TVSTColorList.GetColors: TColorVector;
begin
  ColumnAsColor(Result, 'Цвет');
end;

procedure TVSTColorList.BeforeCellPaint(Sender: TBaseVirtualTree; TargetCanvas: TCanvas;
                              Node: PVirtualNode; Column: TColumnIndex;
                              CellPaintMode: TVTCellPaintMode; CellRect: TRect;
                              var {%H-}ContentRect: TRect);
var
  R: TRect;
begin
  if CellPaintMode<>cpmPaint then Exit;
  if (Node^.Index=SelectedRowIndex) and (FColumnTypes[Column]=ctColor) then
  begin
    TargetCanvas.Brush.Style:= bsSolid;
    TargetCanvas.Brush.Color:= DefaultSelectionBGColor;
    R:= CellRect;
    R.Inflate(FColorColumnCellMargin,FColorColumnCellMargin,
              FColorColumnCellMargin,FColorColumnCellMargin);
    TargetCanvas.Rectangle(R);
  end;
  inherited BeforeCellPaint(Sender, TargetCanvas, Node, Column, CellPaintMode,
                            CellRect, ContentRect);
end;

function TVSTColorList.CellBGColor(Node: PVirtualNode; Column: TColumnIndex): TColor;
begin
  if (Node^.Index=SelectedRowIndex) and (FColumnTypes[Column]<>ctColor) then
    Result:=  DefaultSelectionBGColor
  else
    Result:=inherited CellBGColor(Node, Column);
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
  ValuesClear;
  SetColumnRowTitles(AItems, taLeftJustify);
  SetColumnColor('Цвет', AColors);
  Draw;
end;

end.

