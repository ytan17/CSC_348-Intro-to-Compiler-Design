procedure Hello is 
   begin 
       function Factorial(N)
       begin 
          if N>1 
          then return N * Factorial(N-1); 
          else return 1; 
          end if;
       end;
       main
       begin 
          i := 0; 
          while Factorial(i)<100 loop 
            begin 
              Put_Line(Factorial(i)); 
              i := i+1; 
            end; end loop; 
       end; 
   end;