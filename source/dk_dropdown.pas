unit DK_DropDown;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Graphics, Controls,
  BCButton, BCTypes, BGRABitmap,
  DK_Vector, DK_DropDownForm, DK_DropDownConst, DK_PPI;

type

  { TDropDown }

  TDropDown = class(TObject)
  private
    FButton: TBCButton;
    FItems: TStrVector;
    FForm: TDropDownForm;
    FDropDownCount: Integer;
    FOnChange: TDropDownEvent;
    FDesignTimePPI: Integer;
    FFont: TFont;

    function GetEnabled: Boolean;
    function GetItemIndex: Integer;
    procedure SetButtonSettings;
    procedure ButtonClick(Sender: TObject);
    procedure AfterRenderBCButton(Sender: TObject; const {%H-}ABGRA: TBGRABitmap;
                                  {%H-}AState: TBCButtonState; {%H-}ARect: TRect);
    procedure SetDropDownCount(const AValue: Integer);
    procedure SetEnabled(const AValue: Boolean);
    procedure SetFont(AValue: TFont);
    procedure SetItemIndex(const AValue: Integer);
    procedure SetItems(const AValue: TStrVector);
    procedure DropDownFormDeactivate(Sender: TObject);
    procedure SetOnChange(const AValue: TDropDownEvent);
    function GetSize(const ASize: Integer): Integer;
  public
    constructor Create(const AButton: TBCButton);
    destructor Destroy; override;
    procedure Open;
    procedure Close;
    procedure Clear;
    procedure KeyPick(const APicks: TStrVector; const AKeys: TIntVector; const ASelectedKey: Integer = -1);
    property Items: TStrVector read FItems write SetItems;
    property ItemIndex: Integer read GetItemIndex write SetItemIndex;
    property DropDownCount: Integer read FDropDownCount write SetDropDownCount;
    property Enabled: Boolean read GetEnabled write SetEnabled;
    property OnChange: TDropDownEvent read FOnChange write SetOnChange;
    property Font: TFont read FFont write SetFont;
  end;

implementation

{ TDropDown }

function TDropDown.GetSize(const ASize: Integer): Integer;
begin
  Result:= SizeFromDefaultToDesignTime(ASize, FDesignTimePPI);
end;

procedure TDropDown.SetButtonSettings;

  procedure SetStateCommonSettings(const AState: TBCButtonState);
  begin
    AState.FontEx.Shadow:= False;
    AState.FontEx.Color:= FONT_COLOR_DEFAULT;
    AState.FontEx.Style:= [];
    AState.FontEx.TextAlignment:= bcaLeftCenter;
    AState.FontEx.PaddingLeft:= GetSize(ITEM_MARGIN_DEFAULT);
    AState.Border.Style:= bboSolid;
    AState.Background.Style:= bbsColor;
    AState.Background.Color:= BACKGROUND_COLOR_DEFAULT;
  end;

begin
  FButton.Tag:= -1; //ItemIndex
  FButton.Caption:= EmptyStr;
  FButton.Down:= False;
  FButton.DropDownArrowSize:= GetSize(DROPDOWN_ARROWSIZE_DEFAULT);

  FButton.Height:= GetSize(ITEM_HEIGHT_DEFAULT);
  FButton.Rounding.RoundX:= 0;
  FButton.Rounding.RoundY:= 0;
  FButton.Style:= bbtDropDown;
  FButton.DropDownStyle:= bdsCommon;
  FButton.DropDownWidth:= WidthFromDefaultToScreen(DROPDOWN_WIDTH_DEFAULT);//GetSize(DROPDOWN_WIDTH_DEFAULT);

  SetStateCommonSettings(FButton.StateNormal);
  FButton.StateNormal.Border.Color:= FRAME_COLOR_DEFAULT;

  SetStateCommonSettings(FButton.StateHover);
  FButton.StateHover.Border.Color:= FRAME_COLOR_DEFAULT;

  SetStateCommonSettings(FButton.StateClicked);
  FButton.StateClicked.Border.Color:= FRAME_COLOR_SELECTED;
end;

procedure TDropDown.Open;
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

procedure TDropDown.Close;
begin
  FForm.SetFormHide;
  FButton.Down:= False;
end;

procedure TDropDown.DropDownFormDeactivate(Sender: TObject);
begin
  Close;
end;

procedure TDropDown.SetOnChange(const AValue: TDropDownEvent);
begin
  FOnChange:= AValue;
  FForm.SetOnChange(AValue);
end;

procedure TDropDown.ButtonClick(Sender: TObject);
begin
  FButton.Down:= not FButton.Down;
  if not FButton.Down then Exit;
  Open;
end;

procedure TDropDown.AfterRenderBCButton(Sender: TObject;
  const ABGRA: TBGRABitmap; AState: TBCButtonState; ARect: TRect);
begin
  FForm.SetDropDownText;
end;

procedure TDropDown.SetDropDownCount(const AValue: Integer);
begin
  if FDropDownCount=AValue then Exit;
  FDropDownCount:= AValue;
  FForm.SetDropDownCount(AValue);
end;

procedure TDropDown.SetEnabled(const AValue: Boolean);
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

procedure TDropDown.SetFont(AValue: TFont);
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

function TDropDown.GetEnabled: Boolean;
begin
  Result:= FButton.Enabled;
end;

function TDropDown.GetItemIndex: Integer;
begin
  Result:= FButton.Tag;
end;

procedure TDropDown.SetItemIndex(const AValue: Integer);
begin
  if AValue=FButton.Tag then Exit;
  FButton.Tag:= AValue;
  FForm.SetDropDownText;
  if Assigned(FOnChange) then
    FOnChange;
end;

procedure TDropDown.SetItems(const AValue: TStrVector);
begin
  FItems:= VCut(AValue);
  FForm.SetItems(FItems);
end;

constructor TDropDown.Create(const AButton: TBCButton);
begin
  FDesignTimePPI:= ControlDesignTimePPI(AButton.Parent);

  FButton:= AButton;
  FButton.OnClick:= @ButtonClick;
  FButton.OnAfterRenderBCButton:= @AfterRenderBCButton;
  SetButtonSettings;

  FFont:= TFont.Create;
  FFont.Name:= FButton.StateNormal.FontEx.Name;
  FFont.Height:= -FButton.StateNormal.FontEx.Height;

  FForm:= TDropDownForm.Create(nil);
  FForm.SetButton(FButton);
  FForm.OnDeactivate:= @DropDownFormDeactivate;
  DropDownCount:= DROPDOWN_COUNT_DEFAULT;
end;

destructor TDropDown.Destroy;
begin
  FreeAndNil(FForm);
  FreeAndNil(FFont);
  inherited Destroy;
end;

procedure TDropDown.Clear;
begin
  Items:= nil;
  ItemIndex:= -1;
end;

procedure TDropDown.KeyPick(const APicks: TStrVector; const AKeys: TIntVector;
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

end.

