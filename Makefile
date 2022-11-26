F = gfortran
OPT = -O3
OBJS = value.o fort.o mem_al.o mem_deal.o showfield.o makefield.o fieldtype.o starting.o

%.o: %.f90
	$(F) $(OPT) -c $<
fortranss: $(OBJS)
	$(F) $(OPT) -o $@ $(OBJS)
clean:
	del /f *.o *.mod fortranss.exe