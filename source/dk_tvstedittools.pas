unit DK_TVSTEditTools;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Graphics, Controls, VirtualTrees,
  DK_VSTEdit, DK_Vector;

type

  { TVSTColorList }

  TVSTColorList = class(TVSTEdit)
  private
    procedure NodeClick(Sender: TBaseVirtualTree; const HitInfo: THitInfo);
  public
    constructor Create(const ATree: TVirtualStringTree);
    procedure Update(const AItems: TStrVector; const AColors: TColorVector);
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

end.

