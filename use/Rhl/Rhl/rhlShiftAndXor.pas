unit rhlShiftAndXor;

interface

uses
  rhlCore;

type

  { TrhlShiftAndXor }

  TrhlShiftAndXor = class(TrhlHash)
  private
    m_hash: DWord;
  protected
    procedure UpdateBytes(const ABuffer; ASize: LongWord); override;
  public
    constructor Create; override;
    procedure Init; override;
    procedure Final(var ADigest); override;
  end;

implementation

{ TrhlShiftAndXor }

procedure TrhlShiftAndXor.UpdateBytes(const ABuffer; ASize: LongWord);
var
  b: PByte;
begin
  b := @ABuffer;
  while ASize > 0 do
  begin
    m_hash := m_hash xor ((m_hash shl 5) + (m_hash shr 2) + b^);
    Inc(b);
    Dec(ASize);
  end;
end;

constructor TrhlShiftAndXor.Create;
begin
  HashSize := 4;
  BlockSize := 1;
end;

procedure TrhlShiftAndXor.Init;
begin
  inherited Init;
  m_hash := 0;
end;

procedure TrhlShiftAndXor.Final(var ADigest);
begin
  Move(m_hash, ADigest, 4);
end;

end.
