with calculator;

package body Calculator_Driver is

   task body Calculator_Driver is
      Initialized      : Boolean := False;
      Op1, Op2, Result : Integer;
   begin
      loop
         select
            accept Initialize  do
               Initialized := True;
            end Initialize;
         or when Initialized =>
               accept First_Operand (op : integer) do
                  Op1 := op;
               end First_Operand;
         or when Initialized =>
               accept Second_Operand (op : integer) do
                  Op2 := op;
               end Second_Operand;
         or when Initialized =>
               accept Add  do
                  Result := calculator.Add (Op1, Op2);
               end Add;
         or when Initialized =>
               accept Get_Result (res : out integer) do
                  res := Result;
               end Get_Result;
         or terminate;
         end select;
      end loop;
   end Calculator_Driver;
end Calculator_Driver;
