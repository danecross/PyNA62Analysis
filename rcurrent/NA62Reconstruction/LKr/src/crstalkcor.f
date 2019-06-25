

                        FUNCTION cory(yclus)
c
       implicit none
c
       real yclus
       real ycel
       real cory
       real cy(5)
       data cy/.14684,-.12550E-01,-1.1570,.62721E-01,-4.2877/       
c
c Correct for crosstalk effect
c
          ycel=yclus+128*1.97384
          ycel=(ycel-int(ycel/1.97384)*1.97384)/1.97384-.5 ! y in cell unit
c
         cory=cy(1)
     +       +cy(2)*ycel
     +       +cy(3)*ycel*ycel
     +       +cy(4)*ycel*ycel*ycel
     +       +cy(5)*ycel*ycel*ycel*ycel
c
      end


