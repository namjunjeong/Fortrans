F = gfortran
OPT = -O3
OBJS = value.o fort.o showfield.o makefield.o starting.o testmode.o game.o setplayer.o fire.o move.o bang.o finishgame.o

%.o: %.f90
	$(F) $(OPT) -c $<
fortrans: $(OBJS)
	$(F) $(OPT) -o $@ $(OBJS)
clean:
	del /f *.o *.mod fortrans.exe