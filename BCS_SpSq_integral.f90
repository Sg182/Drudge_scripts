subroutine SpSq_integral(U, V, NAO, &
                        S00, S20p, S20q, S11p, S11q, S02p, S02q, &
                        S40, S31pq, S31qp, S22NN, St22pq, St22qp, S13pq, S13qp, S04)
      use Precision
      implicit none

      integer, intent(in) :: NAO
      complex(kind=pr), intent(in)  :: U(NAO), V(NAO)

      ! All outputs are functions of the external pair (p,q), so all are NAO x NAO
      complex(kind=pr), intent(out) :: S00(NAO,NAO)
      complex(kind=pr), intent(out) :: S20p(NAO,NAO), S20q(NAO,NAO)
      complex(kind=pr), intent(out) :: S11p(NAO,NAO), S11q(NAO,NAO)
      complex(kind=pr), intent(out) :: S02p(NAO,NAO), S02q(NAO,NAO)

      complex(kind=pr), intent(out) :: S40(NAO,NAO)
      complex(kind=pr), intent(out) :: S31pq(NAO,NAO), S31qp(NAO,NAO)
      complex(kind=pr), intent(out) :: S22NN(NAO,NAO)
      complex(kind=pr), intent(out) :: St22pq(NAO,NAO), St22qp(NAO,NAO)
      complex(kind=pr), intent(out) :: S13pq(NAO,NAO), S13qp(NAO,NAO)
      complex(kind=pr), intent(out) :: S04(NAO,NAO)

      integer :: p, q
      complex(kind=pr) :: up, uq, vp, vq
      complex(kind=pr) :: delta_pq
      complex(kind=pr), parameter :: half    = cmplx(0.5_pr,  0.0_pr, kind=pr)
      complex(kind=pr), parameter :: quarter = cmplx(0.25_pr, 0.0_pr, kind=pr)
      complex(kind=pr), parameter :: zero    = cmplx(0.0_pr,  0.0_pr, kind=pr)

      do p = 1, NAO
         up = U(p)
         vp = V(p)

         do q = 1, NAO
            uq = U(q)
            vq = V(q)

            if (p == q) then
               delta_pq = cmplx(1.0_pr, 0.0_pr, kind=pr)
            else
               delta_pq = zero
            end if

            !---------------------------------------------------------------
            ! Scalar part
            !---------------------------------------------------------------
            if (p == q) then
              S00(p,q) = cmplx(0.75_pr, 0.0_pr, kind=pr) !Sp^2 = 3/4 for spin-1/2
            else
              S00(p,q) = up*uq*vp*vq + vp**2*vq**2 - half*vp**2 - half*vq**2 + quarter
            end if
            

            !---------------------------------------------------------------
            ! One-body P^\dagger parts
            !---------------------------------------------------------------
            S20p(p,q) = delta_pq*up**3*vp + half*up**2*uq*vq + up*vp*vq**2 &
                      - half*up*vp - half*uq*vp**2*vq

            S20q(p,q) = delta_pq*up*vp**3 + half*up*uq**2*vp - half*up*vp*vq**2 &
                      + uq*vp**2*vq - half*uq*vq

            !---------------------------------------------------------------
            ! One-body P parts
            ! For real U,V these are the same as O20p/O20q
            !---------------------------------------------------------------
            S02p(p,q) = S20p(p,q)
            S02q(p,q) = S20q(p,q)

            !---------------------------------------------------------------
            ! One-body N parts
            !---------------------------------------------------------------
            S11p(p,q) = -delta_pq*up**2*vp**2 - half*delta_pq*vp**4 + half*up**2*vq**2 &
                      - quarter*up**2 - up*uq*vp*vq - half*vp**2*vq**2 + quarter*vp**2

            S11q(p,q) = -half*delta_pq*vp**4 - up*uq*vp*vq + half*uq**2*vp**2 &
                      - quarter*uq**2 - half*vp**2*vq**2 + quarter*vq**2

            !---------------------------------------------------------------
            ! Two-body NN
            !---------------------------------------------------------------
            S22NN(p,q) = quarter*up**2*uq**2 - quarter*up**2*vq**2 + up*uq*vp*vq &
                       - quarter*uq**2*vp**2 + quarter*vp**2*vq**2

            !---------------------------------------------------------------
            ! Two-body PP and P^\dagger P^\dagger
            !---------------------------------------------------------------
            if (p == q) then
                S04(p,q) = zero
                S40(p,q) = zero
            else
                S04(p,q) = -half*up**2*vq**2 + up*uq*vp*vq - half*uq**2*vp**2
                S40(p,q) = S04(p,q)
            end if
            

            !---------------------------------------------------------------
            ! Two-body P^\dagger P
            !---------------------------------------------------------------
            St22pq(p,q) = half*up**2*uq**2 + up*uq*vp*vq + half*vp**2*vq**2
            St22qp(p,q) = St22pq(p,q)

            !---------------------------------------------------------------
            ! Mixed NP and P^\dagger N
            !---------------------------------------------------------------
            S13pq(p,q) = -half*up**2*uq*vq + half*up*uq**2*vp - half*up*vp*vq**2 &
                       + half*uq*vp**2*vq

            S13qp(p,q) = -S13pq(p,q)

            S31pq(p,q) = -S13pq(p,q)
            S31qp(p,q) =  S13pq(p,q)

         end do
      end do

   end subroutine SpSq_integral
