!     "Next Best Divisor"
!     Given integers N and D with N>D, finds the smallest integer
!     above D which divides N.
      SUBROUTINE NBD(N, D)
      ! IN:       N
      ! IN/OUT:   D
      INTEGER N, D
      ! Local
      INTEGER I, TEST
      
      DO 10 I = D+1, N, 1
        TEST = MOD(N, I)
        IF (TEST.EQ.0) THEN
          D = I
          RETURN
        END IF
   10 CONTINUE
      
      RETURN
      END
