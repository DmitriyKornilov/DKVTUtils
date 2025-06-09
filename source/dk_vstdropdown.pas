unit DK_VSTDropDown;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Graphics, Controls, BCButton, BCTypes, BGRABitmap,
  DK_Vector, DK_VSTDropDownForm, DK_VSTTypes, DK_VSTDropDownConst,
  DK_StrUtils;

type

  { TVSTDropDown }

  TVSTDropDown = class(TObject)
  private
    FButton: TBCButton;
    FItems: TStrVector;
    FForm: TVSTDropDownForm;
    FDropDownCount: Integer;
    FOnChange: TVSTEvent;
    FFont: TFont;

    procedure SetButtonSettings;
    procedure SetDropDownCount(const AValue: Integer);
    procedure SetEnabled(const AValue: Boolean);
    procedure SetFont(AValue: TFont);
    procedure SetItemIndex(const AValue: Integer);
    procedure SetItems(const AValue: TStrVector);
    procedure SetOnChange(const AValue: TVSTEvent);

    function Scale96ToForm(const ASize: Integer): Integer;
    function Scale96ToScreen(const ASize: Integer): Integer;
    function GetEnabled: Boolean;
    function GetItemIndex: Integer;
    function GetText: String;

    procedure ButtonClick(Sender: TObject);
    procedure DoCollapse(Sender: TObject);
    procedure AfterRenderBCButton(Sender: TObject; const {%H-}ABGRA: TBGRABitmap;
                                  {%H-}AState: TBCButtonState; {%H-}ARect: TRect);
  public
    constructor Create(const AButton: TBCButton);
    destructor Destroy; override;

    procedure Clear;
    procedure Expand;
    procedure Collapse;
    procedure KeyPick(const APicks: TStrVector; const AKeys: TIntVector; const ASelectedKey: Integer = -1);
    procedure AutoWidth(const ACaption: String = ''); //use in TForm.OnShow

    property Items: TStrVector read FItems write SetItems;
    property ItemIndex: Integer read GetItemIndex write SetItemIndex;
    property DropDownCount: Integer read FDropDownCount write SetDropDownCount;
    property Enabled: Boolean read GetEnabled write SetEnabled;
    property Font: TFont read FFont write SetFont;
    property Text: String read GetText;

    property OnChange: TVSTEvent read FOnChange write SetOnChange;
  end;

implementation

{ TVSTDropDown }

constructor TVSTDropDown.Create(const AButton: TBCButton);
begin
  FButton:= AButton;
  FButton.OnClick:= @ButtonClick;
  FButton.OnAfterRenderBCButton:= @AfterRenderBCButton;
  SetButtonSettings;

  FFont:= TFont.Create;
  FFont.Name:= FButton.StateNormal.FontEx.Name;
  FFont.Height:= -FButton.StateNormal.FontEx.Height;

  FForm:= TVSTDropDownForm.Create(nil);
  FForm.SetButton(FButton);
  FForm.OnDeactivate:= @DoCollapse;
  DropDownCount:= DROPDOWN_COUNT_DEFAULT;
end;

destructor TVSTDropDown.Destroy;
begin
  FreeAndNil(FForm);
  FreeAndNil(FFont);
  inherited Destroy;
end;

procedure TVSTDropDown.Clear;
begin
  Items:= nil;
  ItemIndex:= -1;
end;

procedure TVSTDropDown.Expand;
var
  P: TPoint;
begin
  FButton.StateNormal.Border.Color:= FRAME_COLOR_SELECTED;
  FButton.StateHover.Border.Color:= FRAME_COLOR_SELECTED;
  P:= Point(0, FButton.Height);
  P:= FButton.ClientToScreen(P);
  FForm.Width:= FButton.Width;
  FForm.Left:= P.X;
  FForm.Top:= P.Y;
  FForm.Show;
end;

procedure TVSTDropDown.Collapse;
begin
  FForm.SetFormHide;
  FButton.Down:= False;
end;

procedure TVSTDropDown.KeyPick(const APicks: TStrVector; const AKeys: TIntVector;
  const ASelectedKey: Integer = -1);
var
  Ind: Integer;
begin
  if VIsNil(APicks) then
  begin
    Clear;
    Exit;
  end;

  Items:= APicks;
  if ASelectedKey>=0 then
  begin
    Ind:= VIndexOf(AKeys, ASelectedKey);
    if Ind<0 then Ind:= 0;
    ItemIndex:= Ind;
  end
  else
    ItemIndex:= 0;
end;

procedure TVSTDropDown.AutoWidth(const ACaption: String = ''); //use in TForm.OnShow
var
  W: Integer;
begin
  if SEmpty(ACaption) then
    W:= VMaxWidth(FItems, Font, FButton.Width)
  else
    W:= SWidth(ACaption, Font);
  FButton.Width:= Scale96ToForm(W + DROPDOWN_WIDTH_DEFAULT + 2*ITEM_MARGIN_DEFAULT);
end;

procedure TVSTDropDown.SetButtonSettings;

  procedure SetStateCommonSettings(const AState: TBCButtonState);
  begin
    AState.FontEx.Shadow:= False;
    AState.FontEx.Color:= FONT_COLOR_DEFAULT;
    AState.FontEx.Style:= [];
    AState.FontEx.TextAlignment:= bcaLeftCenter;
    AState.FontEx.PaddingLeft:= Scale96ToForm(ITEM_MARGIN_DEFAULT);
    AState.Border.Style:= bboSolid;
    AState.Background.Style:= bbsColor;
    AState.Background.Color:= BACKGROUND_COLOR_DEFAULT;
  end;

begin
  FButton.Tag:= -1; //ItemIndex
  FButton.Caption:= EmptyStr;
  FButton.Down:= False;
  FButton.DropDownArrowSize:= Scale96ToForm(DROPDOWN_ARROWSIZE_DEFAULT);

  FButton.Height:= Scale96ToForm(ITEM_HEIGHT_DEFAULT);
  FButton.Rounding.RoundX:= 0;
  FButton.Rounding.RoundY:= 0;
  FButton.Style:= bbtDropDown;
  FButton.DropDownStyle:= bdsCommon;
  FButton.DropDownWidth:= Scale96ToScreen(DROPDOWN_WIDTH_DEFAULT);

  SetStateCommonSettings(FButton.StateNormal);
  FButton.StateNormal.Border.Color:= FRAME_COLOR_DEFAULT;

  SetStateCommonSettings(FButton.StateHover);
  FButton.StateHover.Border.Color:= FRAME_COLOR_DEFAULT;

  SetStateCommonSettings(FButton.StateClicked);
  FButton.StateClicked.Border.Color:= FRAME_COLOR_SELECTED;
end;

procedure TVSTDropDown.SetDropDownCount(const AValue: Integer);
begin
  if FDropDownCount=AValue then Exit;
  FDropDownCount:= AValue;
  FForm.SetDropDownCount(AValue);
end;

procedure TVSTDropDown.SetEnabled(const AValue: Boolean);
begin
  if FButton.Enabled=AValue then Exit;
  FButton.Enabled:= AValue;

  if AValue then
  begin
    FButton.StateNormal.FontEx.Color:= FONT_COLOR_DEFAULT;
    FButton.StateNormal.Background.Color:= BACKGROUND_COLOR_DEFAULT;
    FButton.StateNormal.Border.Color:= FRAME_COLOR_DEFAULT;
  end
  else begin
    FButton.StateNormal.FontEx.Color:= FONT_COLOR_DISABLED;
    FButton.StateNormal.Background.Color:= BACKGROUND_COLOR_DISABLED;
    FButton.StateNormal.Border.Color:= FRAME_COLOR_DISABLED;
  end;
end;

procedure TVSTDropDown.SetFont(AValue: TFont);
begin
  FFont.Assign(AValue);
  FForm.SetListFont(AValue);
  FButton.StateNormal.FontEx.Name:= AValue.Name;
  FButton.StateNormal.FontEx.Height:= -AValue.Height;
  FButton.StateHover.FontEx.Name:= AValue.Name;
  FButton.StateHover.FontEx.Height:= -AValue.Height;
  FButton.StateClicked.FontEx.Name:= AValue.Name;
  FButton.StateClicked.FontEx.Height:= -AValue.Height;
end;

procedure TVSTDropDown.SetItemIndex(const AValue: Integer);
begin
  if AValue=FButton.Tag then Exit;
  FButton.Tag:= AValue;
  FForm.SetDropDownText;
  if Assigned(FOnChange) then
    FOnChange;
end;

procedure TVSTDropDown.SetItems(const AValue: TStrVector);
begin
  FItems:= VCut(AValue);
  FForm.SetItems(FItems);
end;

procedure TVSTDropDown.SetOnChange(const AValue: TVSTEvent);
begin
  FOnChange:= AValue;
  FForm.SetOnChange(AValue);
end;

function TVSTDropDown.Scale96ToForm(const ASize: Integer): Integer;
begin
  Result:= FButton.Parent.Scale96ToForm(ASize);
end;

function TVSTDropDown.Scale96ToScreen(const ASize: Integer): Integer;
begin
  Result:= FButton.Parent.Scale96ToScreen(ASize);
end;

function TVSTDropDown.GetEnabled: Boolean;
begin
  Result:= FButton.Enabled;
end;

function TVSTDropDown.GetItemIndex: Integer;
begin
  Result:= FButton.Tag;
end;

function TVSTDropDown.GetText: String;
begin
  Result:= EmptyStr;
  if VIsNil(Items) then Exit;
  Result:= Items[FButton.Tag];
end;

procedure TVSTDropDown.ButtonClick(Sender: TObject);
begin
  FButton.Down:= not FButton.Down;
  if not FButton.Down then Exit;
  Expand;
end;

procedure TVSTDropDown.DoCollapse(Sender: TObject);
begin
  Collapse;
end;

procedure TVSTDropDown.AfterRenderBCButton(Sender: TObject;
  const ABGRA: TBGRABitmap; AState: TBCButtonState; ARect: TRect);
begin
  FForm.SetDropDownText;
end;

end.

