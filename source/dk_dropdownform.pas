unit DK_DropDownForm;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, VirtualTrees,
  BCButton,
  DK_Vector, DK_StrUtils, DK_VSTTableTools, DK_DropDownConst;

type
  TDropDownEvent = procedure of object;

  { TDropDownForm }

  TDropDownForm = class(TForm)
    Shape: TShape;
    VT: TVirtualStringTree;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure VTMouseMove(Sender: TObject; {%H-}Shift: TShiftState; X, Y: Integer);
    procedure VTNodeClick(Sender: TBaseVirtualTree; const {%H-}HitInfo: THitInfo);
  private

    FButton: TBCButton;
    FItems: TStrVector;
    FList: TVSTStringList;
    FOnChange: TDropDownEvent;

    procedure ItemChoose;
  public
    procedure SetFormHide;
    procedure SetItems(const AItems: TStrVector);
    procedure SetButton(const AButton: TBCButton);
    procedure SetListFont(const AFont: TFont);
    procedure SetDropDownText;
    procedure SetDropDownCount(const AValue: Integer);
    procedure SetOnChange(const AOnChange: TDropDownEvent);
  end;

var
  DropDownForm: TDropDownForm;

implementation


{$R *.lfm}




{ TDropDownForm }

procedure TDropDownForm.FormCreate(Sender: TObject);
begin
  FList:= TVSTStringList.Create(VT, EmptyStr, nil);
  FList.OnReturnKeyDown:= @ItemChoose;
  FList.AutoHeight:= True;
end;

procedure TDropDownForm.FormDestroy(Sender: TObject);
begin
  FreeAndNil(FList);
end;

procedure TDropDownForm.FormShow(Sender: TObject);
begin
  Height:= FList.TotalHeight + 2 {shape borders};
end;

procedure TDropDownForm.VTMouseMove(Sender: TObject; Shift: TShiftState; X,Y: Integer);
var
  Node: PVirtualNode;
begin
  Node:= VT.GetNodeAt(X,Y);
  if Assigned(Node) and (Node^.Index<>FList.SelectedIndex) then
    FList.Select(Node^.Index);
end;

procedure TDropDownForm.VTNodeClick(Sender: TBaseVirtualTree; const HitInfo: THitInfo);
begin
  ItemChoose;
end;

procedure TDropDownForm.SetFormHide;
begin
  FButton.StateNormal.Border.Color:= FRAME_COLOR_DEFAULT;
  FButton.StateHover.Border.Color:= FRAME_COLOR_DEFAULT;
  Hide;
end;

procedure TDropDownForm.ItemChoose;
begin
  if FList.IsSelected then
  begin
    FButton.Tag:= FList.SelectedIndex;
    SetDropDownText;
    if Assigned(FOnChange) then
      FOnChange;
  end;
  SetFormHide;
end;

procedure TDropDownForm.SetItems(const AItems: TStrVector);
begin
  FItems:= AItems;
  FList.Update(FItems);
end;

procedure TDropDownForm.SetButton(const AButton: TBCButton);
begin
  FButton:= AButton;
  Font.Name:= FButton.StateNormal.FontEx.Name;
  Font.Height:= FButton.StateNormal.FontEx.Height;
  Font.Style:= FButton.StateNormal.FontEx.Style;
end;

procedure TDropDownForm.SetListFont(const AFont: TFont);
begin
  FList.SetSingleFont(AFont);
end;

procedure TDropDownForm.SetDropDownText;
var
  MaxWidth, i: Integer;
  FullText, CutText, NextSymbol: String;
begin
  FullText:= EmptyStr;
  if FButton.Tag>=0 then
    FullText:= FItems[FButton.Tag];

  MaxWidth:= FButton.Width - FButton.DropDownWidth - ITEM_MARGIN_DEFAULT;

  if SWidth(FullText, Font)<=MaxWidth then
    FButton.Caption:= FullText
  else begin
    MaxWidth:= MaxWidth - SWidth(ITEM_CUT_SYMBOL, Font);
    CutText:= SSymbol(FullText, 1);
    for i:=2 to SLength(FullText) do
    begin
      NextSymbol:= SSymbol(FullText, i);
      if SWidth(CutText+NextSymbol, Font)>MaxWidth then
        break
      else
        CutText:= CutText+NextSymbol;
    end;
    FButton.Caption:= CutText + ITEM_CUT_SYMBOL;
  end;
end;

procedure TDropDownForm.SetDropDownCount(const AValue: Integer);
begin
  FList.MaxAutoHeightRowCount:= AValue;
end;

procedure TDropDownForm.SetOnChange(const AOnChange: TDropDownEvent);
begin
  FOnChange:= AOnChange;
end;

end.

