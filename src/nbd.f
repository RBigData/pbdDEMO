!     "Next Best Divisor"
!     Given integers N and D with N>D, finds the smallest integer
!     above D which divides N.
      SUBROUTINE NBD(N, D)
      ! IN/OUT
      INTEGER N, D
      ! Local
      INTEGER I, TEST
      
      DO I = D+1, N-1, 1
        TEST = MOD(N, I)
        IF (TEST.EQ.0) THEN
          D = I
          RETURN
        END IF
      END DO
      
      D = N
      RETURN
      END

