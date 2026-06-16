#include <stdlib.h>
#include <stdio.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <unistd.h>
#include <mpi.h>

int main(int argc, char *argv[]) {
  FILE *fhandle, *mfhandle;
  int mysize, mpi_rank, ret = 1;
  char line1[10000];
  char line2[10000];
  struct stat st = {0};
  void *ptr = malloc(1024*1024*1024);
  MPI_Init(&argc, &argv);
  MPI_Comm_rank(MPI_COMM_WORLD, &mpi_rank);
  if (stat(argv[1], &st) == -1) {
    mkdir(argv[1], 0700);
  }
  if (mpi_rank == 0) {
    mfhandle = fopen(argv[2], "r");
  }
  while (ret > 0) {
    if (mpi_rank == 0) {
      ret = fscanf(mfhandle, "%s %s", line1, line2);
    }
    MPI_Bcast(&ret, 1, MPI_INT, 0, MPI_COMM_WORLD);
    if (ret > 0) {
      MPI_Bcast(line1, 10000, MPI_CHAR, 0, MPI_COMM_WORLD);
      MPI_Bcast(line2, 10000, MPI_CHAR, 0, MPI_COMM_WORLD);
      if (mpi_rank == 0) {
        fhandle = fopen(line1, "r");
        if (fhandle) {
          mysize = fread(ptr, 1, 1024*1024*1024, fhandle);
          fclose(fhandle);
        } else {
          mysize = 0;
        }
      }
      MPI_Bcast(&mysize, 1, MPI_INT, 0, MPI_COMM_WORLD);
      if (mysize > 0 && mysize < 1024*1024*1024) {
        MPI_Bcast(ptr, mysize, MPI_CHAR, 0, MPI_COMM_WORLD);
        fhandle = fopen(line2, "w");
        if (fhandle) {
          fwrite(ptr, 1, mysize, fhandle);
          fclose(fhandle);
        }
      }
    }
  }
  MPI_Finalize();
  return 0;
}
