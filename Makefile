all:
	gfortran -c src/caesarShift.f90 -Jmod -Imod -o obj/caesarShift.o
	gfortran -c src/tests.f90 -Jmod -Imod -o obj/test.o
	gfortran -c src/main.f90 -Jmod -Imod -o obj/main.o
	gfortran obj/* -o bin/target

clean:
	rm mod/* obj/* bin/*