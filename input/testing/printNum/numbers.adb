procedure Numbers is 
   begin 
       function Test(a,b,c,d,e,f)
       begin 
          Put_Line(a);
          Put_Line(b);
          Put_Line(c);
          Put_Line(d);
          Put_Line(e);
          Put_Line(f);
       end;
       main
       begin 
          String := Test (0-2, 0-1, 0, 1, 2, 3);
       end; 
   end;