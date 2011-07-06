package Calculator_Driver is

   task Calculator_Driver is

      entry Initialize;
      entry First_Operand  (op : integer);
      entry Second_Operand (op : integer);
      entry Add;
      entry Get_Result (res : out integer);

   end Calculator_Driver;

end Calculator_Driver;
