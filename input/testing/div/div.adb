procedure Div is 
   begin 
       main
       begin 
          i := 0;
          while i < 100 loop 
            begin 
              n := i mod 3
              m := i mod 5
              if n > 0 then
                if m > 0 then
                  Put_Line(i);
                else b := 0; end if;
              else b := 0; end if;
              i := i+1;
            end; end loop; 
       end; 
   end;