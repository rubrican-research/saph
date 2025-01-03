unit rhlGostCryptoPro;

interface

uses
  rhlCore, rhlGostBase;

type

  { TrhlGostCryptoPro }

  TrhlGostCryptoPro = class(TrhlGostBase)
  public
    constructor Create; override;
  end;

implementation

{ TrhlGostCryptoPro }

constructor TrhlGostCryptoPro.Create;
const
  csbox: array[0..7, 0..15] of DWord =
    (
      (10, 4, 5, 6, 8, 1, 3, 7, 13, 12, 14, 0, 9, 2, 11, 15),
      (5, 15, 4, 0, 2, 13, 11, 9, 1, 7, 6, 3, 12, 14, 10, 8),
      (7, 15, 12, 14, 9, 4, 1, 0, 3, 11, 5, 2, 6, 10, 8, 13),
      (4, 10, 7, 12, 0, 15, 2, 8, 14, 1, 6, 5, 13, 11, 9, 3),
      (7, 6, 4, 11, 9, 12, 2, 10, 1, 8, 0, 14, 15, 13, 3, 5),
      (7, 6, 2, 4, 13, 9, 15, 0, 10, 1, 5, 11, 8, 14, 12, 3),
      (13, 14, 4, 1, 7, 0, 5, 10, 3, 12, 8, 15, 6, 2, 9, 11),
      (1, 3, 10, 9, 5, 11, 4, 15, 8, 6, 7, 14, 13, 0, 2, 12)
    );
begin
  Move(csbox, sbox, SizeOf(csbox));
  inherited;
end;

end.
