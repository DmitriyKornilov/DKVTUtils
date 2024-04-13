unit DK_VSTDropDownForm;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, VirtualTrees,
  BCButton,
  DK_Vector, DK_StrUtils, DK_VSTTypes, DK_VSTTableTools, DK_VSTDropDownConst;

type

  { TVSTDropDownForm }

  TVSTDropDownForm = class(TForm)
    Shape: TShape;
    VT: TVirtualStringTree;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure VTMouseMove(Sender: TObject; {%H-}Shift: TShiftState; X, Y: Integer);
    procedure VTNodeClick(Sender: TBaseVirtualTree; const {%H-}HitInfo: THitInfo);
  private
    Button: TBCButton;
    Items: TStrVector;
    List: TVSTStringList;
    OnChange: TVSTEvent;
    procedure ItemChoose;
  public
    procedure SetFormHide;
    procedure SetItems(const AItems: TStrVector);
    procedure SetButton(const AButton: TBCButton);
    procedure SetListFont(const AFont: TFont);
    procedure SetDropDownText;
    procedure SetDropDownCount(const AValue: Integer);
    procedure SetOnChange(const AOnChange: TVSTEvent);
  end;

var
  VSTDropDownForm: TVSTDropDownForm;

implementation

{$R *.lfm}

{ TVSTDropDownForm }

procedure TVSTDropDownForm.FormCreate(Sender: TObject);
begin
  List:= TVSTStringList.Create(VT, EmptyStr, nil);
  List.OnReturnKeyDown:= @ItemChoose;
  List.AutoHeight:= True;
end;

procedure TVSTDropDownForm.FormDestroy(Sender: TObject);
begin
  FreeAndNil(List);
end;

procedure TVSTDropDownForm.FormShow(Sender: TObject);
begin
  Height:= List.TotalHeight + 2 {shape borders};
end;

procedure TVSTDropDownForm.VTMouseMove(Sender: TObject; Shift: TShiftState; X,Y: Integer);
var
  Node: PVirtualNode;
begin
  Node:= VT.GetNodeAt(X,Y);
  if Assigned(Node) and (Node^.Index<>List.SelectedIndex) then
    List.Select(Node^.Index);
end;

procedure TVSTDropDownForm.VTNodeClick(Sender: TBaseVirtualTree; const HitInfo: THitInfo);
begin
  ItemChoose;
end;

procedure TVSTDropDownForm.ItemChoose;
begin
  if List.IsSelected then
  begin
    Button.Tag:= List.SelectedIndex;
    SetDropDownText;
    if Assigned(OnChange) then
      OnChange;
  end;
  SetFormHide;
end;

procedure TVSTDropDownForm.SetFormHide;
begin
  Button.StateNormal.Border.Color:= FRAME_COLOR_DEFAULT;
  Button.StateHover.Border.Color:= FRAME_COLOR_DEFAULT;
  Hide;
end;

procedure TVSTDropDownForm.SetItems(const AItems: TStrVector);
begin
  Items:= AItems;
  List.Update(Items);
end;

procedure TVSTDropDownForm.SetButton(const AButton: TBCButton);
begin
  Button:= AButton;
  Font.Name:= Button.StateNormal.FontEx.Name;
  Font.Height:= Button.StateNormal.FontEx.Height;
  Font.Style:= Button.StateNormal.FontEx.Style;
end;

procedure TVSTDropDownForm.SetListFont(const AFont: TFont);
begin
  List.SetSingleFont(AFont);
end;

procedure TVSTDropDownForm.SetDropDownText;
var
  MaxWidth, i: Integer;
  FullText, CutText, NextSymbol: String;
begin
  FullText:= EmptyStr;
  if Button.Tag>=0 then
    FullText:= Items[Button.Tag];

  MaxWidth:= Button.Width - Button.DropDownWidth - ITEM_MARGIN_DEFAULT;

  if SWidth(FullText, Font)<=MaxWidth then
    Button.Caption:= FullText
  else begin
    MaxWidth:= MaxWidth - SWidth(ITEM_CUT_SYMBOL, Font);
    CutText:= SSymbol(FullText, 1);
    for i:=2 to SLength(FullText) do
    begin
      NextSymbol:= SSymbol(FullText, i);
      if SWidth(CutText+NextSymbol, Font)>MaxWidth then
        break
      else
        CutText:= CutText + NextSymbol;
    end;
    Button.Caption:= CutText + ITEM_CUT_SYMBOL;
  end;
end;

procedure TVSTDropDownForm.SetDropDownCount(const AValue: Integer);
begin
  List.MaxAutoHeightRowCount:= AValue;
end;

procedure TVSTDropDownForm.SetOnChange(const AOnChange: TVSTEvent);
begin
  OnChange:= AOnChange;
end;

end.

