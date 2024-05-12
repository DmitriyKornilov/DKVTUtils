unit DK_VSTParamList;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Controls, ExtCtrls, StdCtrls, Forms, VirtualTrees,
  Graphics, DK_VSTTypes, DK_VSTTables, DK_VSTTableTools, DK_Vector;

const
  SCROLLBAR_WIDTH_DEFAULT = 16;
  SCROLLBAR_DELTA_DEFAULT = 20;
  ITEM_SPACE_DEFAULT = 6;

type
  TVSTListType = (ltString, ltCheck);

  { TVSTParamList }

  TVSTParamList = class (TObject)
  private
    FParent: TPanel;
    FPanel: TPanel;
    FTrees: array of TVirtualStringTree;
    FItems: array of TVSTSimpleTable;
    FTypes: array of TVSTListType;
    FNames: TStrVector;
    FScrollBar: TScrollBar;
    FSpace: Integer;
    FHeight: Integer;
    FFont: TFont;




    function ItemIndex(const AItemName: String): Integer;

    function GetSelectedByIndex(const AItemIndex: Integer): Integer;
    function GetSelected(const AItemName: String): Integer;
    function GetCheckedByIndex(const AItemIndex: Integer): TBoolVector;
    function GetChecked(const AItemName: String): TBoolVector;
    function GetCheckedIntByIndex(const AItemIndex: Integer): TIntVector;
    function GetCheckedInt(const AItemName: String): TIntVector;
    function GetParams: TIntVector;



    procedure SetSelectedByIndex(const AItemIndex: Integer; const AValue: Integer);
    procedure SetSelected(const AItemName: String; const AValue: Integer);
    procedure SetCheckedByIndex(const AItemIndex: Integer; const AValue: TBoolVector);
    procedure SetChecked(const AItemName: String; const AValue: TBoolVector);
    procedure SetCheckedIntByIndex(const AItemIndex: Integer; const AValue: TIntVector);
    procedure SetCheckedInt(const AItemName: String; const AValue: TIntVector);
    procedure SetParams(const AValue: TIntVector);

    procedure MouseWheel(Sender: TObject; {%H-}Shift: TShiftState; WheelDelta: Integer;
                         {%H-}MousePos: TPoint; var {%H-}Handled: Boolean);
    procedure ScrollBarChange(Sender: TObject);
    procedure ChangeBounds(Sender: TObject);
    procedure ResizeControls;
    procedure AddCustomList(const AListType: TVSTListType; const AName: String);

    property SelectedByIndex[const AItemIndex: Integer]: Integer read GetSelectedByIndex write SetSelectedByIndex;
    property CheckedByIndex[const AItemIndex: Integer]: TBoolVector read GetCheckedByIndex write SetCheckedByIndex;
    property CheckedIntByIndex[const AItemIndex: Integer]: TIntVector read GetCheckedIntByIndex write SetCheckedIntByIndex;

  public
    constructor Create(const AParent: TPanel; const AFont: TFont = nil);
    destructor Destroy; override;

    procedure Show; //use in TForm.OnShow

    procedure AddStringList(const AName, ACaption: String;
                            const AItems: TStrVector;
                            const AOnSelect: TVSTEvent;
                            const ASelectedIndex: Integer = 0);
    procedure AddCheckList(const AName, ACaption: String;
                            const AItems: TStrVector;
                            const AOnSelect: TVSTEvent;
                            const ACheckedCount: Integer = -1); //-1 check all, >=0 check [0..ACheckedCount-1]

    property Selected[const AItemName: String]: Integer read GetSelected write SetSelected;
    property Checked[const AItemName: String]: TBoolVector read GetChecked write SetChecked;
    property CheckedInt[const AItemName: String]: TIntVector read GetCheckedInt write SetCheckedInt;
    property Params: TIntVector read GetParams write SetParams;

    property Height: Integer read FHeight;
  end;

implementation

{ TVSTParamList }

function TVSTParamList.ItemIndex(const AItemName: String): Integer;
begin
  Result:= VIndexOf(FNames, AItemName);
end;

function TVSTParamList.GetCheckedIntByIndex(const AItemIndex: Integer): TIntVector;
begin
  Result:= VBoolToInt(CheckedByIndex[AItemIndex]);
end;

function TVSTParamList.GetCheckedInt(const AItemName: String): TIntVector;
begin
  Result:= VBoolToInt(Checked[AItemName]);
end;

procedure TVSTParamList.SetCheckedIntByIndex(const AItemIndex: Integer; const AValue: TIntVector);
begin
  CheckedByIndex[AItemIndex]:= VIntToBool(AValue);
end;

procedure TVSTParamList.SetCheckedInt(const AItemName: String; const AValue: TIntVector);
begin
  Checked[AItemName]:= VIntToBool(AValue);
end;

function TVSTParamList.GetParams: TIntVector;
var
  i: Integer;
begin
  Result:= nil;
  if VIsNil(FNames) then Exit;
  for i:= 0 to High(FNames) do
  begin
    case FTypes[i] of
      ltString: VAppend(Result, SelectedByIndex[i]);
      ltCheck:  Result:= VAdd(Result, CheckedIntByIndex[i]);
    end;
  end;
end;

procedure TVSTParamList.SetParams(const AValue: TIntVector);
var
  i, j, n: Integer;
begin
  if VIsNil(FNames) then Exit;
  j:= 0;
  for i:= 0 to High(FNames) do
  begin
    case FTypes[i] of
      ltString:
        begin
          n:= 1;
          SelectedByIndex[i]:= AValue[j];
        end;
      ltCheck:
        begin
          n:= (FItems[i] as TVSTCheckList).Count;
          CheckedIntByIndex[i]:= VCut(AValue, j, j+n-1);
        end;
    end;
    j:= j + n;
  end;
end;

function TVSTParamList.GetCheckedByIndex(const AItemIndex: Integer): TBoolVector;
begin
  Result:= nil;
  if (AItemIndex<0) or (FTypes[AItemIndex]<>ltCheck) then Exit;
  Result:= (FItems[AItemIndex] as TVSTCheckList).Selected;
end;

function TVSTParamList.GetChecked(const AItemName: String): TBoolVector;
begin
  Result:= CheckedByIndex[ItemIndex(AItemName)];
end;

procedure TVSTParamList.SetCheckedByIndex(const AItemIndex: Integer; const AValue: TBoolVector);
begin
  if (AItemIndex<0) or (FTypes[AItemIndex]<>ltCheck) then Exit;
  (FItems[AItemIndex] as TVSTCheckList).Selected:= AValue;
end;

procedure TVSTParamList.SetChecked(const AItemName: String; const AValue: TBoolVector);
begin
  CheckedByIndex[ItemIndex(AItemName)]:= AValue;
end;

function TVSTParamList.GetSelectedByIndex(const AItemIndex: Integer): Integer;
begin
  Result:= -1;
  if (AItemIndex<0) or (FTypes[AItemIndex]<>ltString) then Exit;
  Result:= (FItems[AItemIndex] as TVSTStringList).ItemIndex;
end;

function TVSTParamList.GetSelected(const AItemName: String): Integer;
begin
  Result:= SelectedByIndex[ItemIndex(AItemName)];
end;

procedure TVSTParamList.SetSelectedByIndex(const AItemIndex: Integer; const AValue: Integer);
begin
  if (AItemIndex<0) or (FTypes[AItemIndex]<>ltString) then Exit;
  (FItems[AItemIndex] as TVSTStringList).ItemIndex:= AValue;
end;

procedure TVSTParamList.SetSelected(const AItemName: String; const AValue: Integer);
begin
  SelectedByIndex[ItemIndex(AItemName)]:= AValue;
end;

procedure TVSTParamList.MouseWheel(Sender: TObject; Shift: TShiftState;
  WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
var
  H: Integer;
begin
  if not FScrollBar.Visible then Exit;
  H:= FParent.Scale96ToForm(SCROLLBAR_DELTA_DEFAULT);
  if WheelDelta<0 then
    FScrollBar.Position:= FScrollBar.Position + H
  else
    FScrollBar.Position:= FScrollBar.Position - H;
end;

procedure TVSTParamList.ScrollBarChange(Sender: TObject);
begin
  FPanel.Top:= -FScrollBar.Position;
end;

procedure TVSTParamList.ChangeBounds(Sender: TObject);
begin
  ResizeControls;
end;

procedure TVSTParamList.ResizeControls;
var
  i: Integer;
begin
  FScrollBar.Visible:= FParent.ClientHeight<FHeight;
  if FScrollBar.Visible then
    FScrollBar.Max:= FHeight - FParent.ClientHeight;

  FPanel.Width:= FParent.ClientWidth - FScrollBar.Width*Ord(FScrollBar.Visible);
  for i:= 0 to High(FTrees) do
    FTrees[i].Width:= FPanel.ClientWidth;
end;

constructor TVSTParamList.Create(const AParent: TPanel; const AFont: TFont = nil);
begin
  inherited Create;

  FParent:= AParent;
  FParent.OnChangeBounds:= @ChangeBounds;
  FHeight:= 0;
  FSpace:= FParent.Scale96ToForm(ITEM_SPACE_DEFAULT);

  FFont:= TFont.Create;
  if Assigned(AFont) then
    FFont.Assign(AFont)
  else
    FFont.Assign(FParent.Font);

  FScrollBar:= TScrollBar.Create(nil);
  FScrollBar.OnChange:= @ScrollBarChange;
  FScrollBar.Parent:= FParent;
  FScrollBar.Kind:= sbVertical;
  FScrollBar.Align:= alRight;
  FScrollBar.Width:= FParent.Scale96ToForm(SCROLLBAR_WIDTH_DEFAULT);
  FScrollBar.Visible:= False;

  FPanel:= TPanel.Create(nil);
  FPanel.OnMouseWheel:= @MouseWheel;
  FPanel.Parent:= FParent;
  FPanel.Color:= clWindow;
  FPanel.BevelOuter:= bvNone;
  FPanel.BevelInner:= bvNone;
  FPanel.BorderStyle:= bsNone;
  FPanel.Left:= 0;
  FPanel.Top:= 0;
  FPanel.Width:= FParent.ClientWidth;
  FPanel.Height:= FParent.ClientHeight;
end;

destructor TVSTParamList.Destroy;
var
  i: Integer;
begin
  for i:= 0 to High(FItems) do
    FreeAndNil(FItems[i]);
  FreeAndNil(FPanel);
  FreeAndNil(FScrollBar);
  FreeAndNil(FFont);

  inherited Destroy;
end;

procedure TVSTParamList.Show;
var
  i: Integer;
begin
  FHeight:= 0;
  if Length(FTrees)=0 then Exit;
  for i:= 0 to High(FItems) do
  begin
    FTrees[i].Top:= FHeight + FSpace*Ord(i>0);
    FHeight:= FHeight + FItems[i].TotalHeight + FSpace*Ord(i>0);
  end;
  FPanel.Height:= FHeight;
  //ResizeControls;
end;

procedure TVSTParamList.AddCustomList(const AListType: TVSTListType; const AName: String);
var
  VT: TVirtualStringTree;
  N: Integer;
begin
  N:= Length(FTrees);
  SetLength(FTrees, N+1);
  VT:= TVirtualStringTree.Create(FPanel);
  VT.Parent:= FPanel;
  VT.Left:= 0;
  VT.OnMouseWheel:= @MouseWheel;
  FTrees[N]:= VT;

  SetLength(FTypes, N+1);
  FTypes[N]:= AListType;
  VAppend(FNames, AName);
end;

procedure TVSTParamList.AddStringList(const AName, ACaption: String;
                            const AItems: TStrVector;
                            const AOnSelect: TVSTEvent;
                            const ASelectedIndex: Integer = 0);
var
  N: Integer;
  List: TVSTStringList;
begin
  AddCustomList(ltString, AName);

  N:= Length(FItems);
  SetLength(FItems, N+1);
  List:= TVSTStringList.Create(FTrees[N], ACaption, AOnSelect);
  List.Update(AItems, ASelectedIndex);
  FItems[N]:= List;
end;

procedure TVSTParamList.AddCheckList(const AName, ACaption: String;
                            const AItems: TStrVector;
                            const AOnSelect: TVSTEvent;
                            const ACheckedCount: Integer = -1);
var
  N: Integer;
  List: TVSTCheckList;
begin
  AddCustomList(ltCheck, AName);

  N:= Length(FItems);
  SetLength(FItems, N+1);
  List:= TVSTCheckList.Create(FTrees[N], ACaption, AItems, AOnSelect, ACheckedCount);
  FItems[N]:= List;
end;

end.
