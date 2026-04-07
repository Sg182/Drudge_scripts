subroutine Hub2PairComplex(x, y, h, V, W, NAO, &
     H00, H20, H11, H02, H40, H31, H22, Ht22, H13, H04)

      use Precision
      implicit none

      integer, intent(in) :: NAO
      complex(kind=pr), intent(in)  :: x(NAO), y(NAO)
      complex(kind=pr), intent(in)  :: h(NAO)
      complex(kind=pr), intent(in)  :: V(NAO,NAO), W(NAO,NAO)

      complex(kind=pr), intent(out) :: H00
      complex(kind=pr), intent(out) :: H20(NAO), H11(NAO), H02(NAO)
      complex(kind=pr), intent(out) :: H40(NAO,NAO), H31(NAO,NAO)
      complex(kind=pr), intent(out) :: H22(NAO,NAO), Ht22(NAO,NAO)
      complex(kind=pr), intent(out) :: H13(NAO,NAO), H04(NAO,NAO)

      integer :: p, q
      complex(kind=pr) :: xp, yp, xq, yq
      complex(kind=pr) :: cxp, cyp, cxq, cyq
      complex(kind=pr) :: z0, tmp

      z0 = cmplx(0.0_pr, 0.0_pr, kind=pr)

      H00  = z0
      H20  = z0
      H11  = z0
      H02  = z0
      H40  = z0
      H31  = z0
      H22  = z0
      Ht22 = z0
      H13  = z0
      H04  = z0

      do p = 1, NAO

         xp  = x(p)
         yp  = y(p)
         cxp = conjg(xp)
         cyp = conjg(yp)

         ! H11(p) * N_p
         H11(p) = cxp*h(p)*xp - cyp*cyp*V(p,p)*yp*yp - cyp*h(p)*yp

         ! H20(p) * P_p
         H20(p) = 2.0_pr*cxp*cyp*V(p,p)*yp*yp + 2.0_pr*cxp*h(p)*yp

         ! H02(p) * Pdag_p
         H02(p) = 2.0_pr*cyp*cyp*V(p,p)*xp*yp + 2.0_pr*cyp*h(p)*xp

         ! Explicit one-body scalar contribution
         H00 = H00 + cyp*cyp*V(p,p)*yp*yp + 2.0_pr*cyp*h(p)*yp

         do q = 1, NAO

            xq  = x(q)
            yq  = y(q)
            cxq = conjg(xq)
            cyq = conjg(yq)

            ! Scalar part
            H00 = H00 + cxq*cyq*V(p,q)*xp*yp + cyp*cyq*W(p,q)*yp*yq

            ! H40(p,q) : Pdag_p Pdag_q  (raw, symmetrized later)
            H40(p,q) = cyp*cyq*W(p,q)*xp*xq - cyq*cyq*V(p,q)*xp*xp

            ! Ht22(p,q) : Pdag_p P_q
            Ht22(p,q) = cxq*cxq*V(p,q)*xp*xp                           &
                      + 2.0_pr*cxq*cyp*W(p,q)*xp*yq                    &
                      + cyp*cyp*V(q,p)*yq*yq

            ! Extra contribution to H02(p) : Pdag_p
            H02(p) = H02(p)                                            &
                  + cxq*cyq*V(p,q)*xp*xp                               &
                  - cyp*cyp*V(q,p)*xq*yq                               &
                  + 2.0_pr*cyp*cyq*W(p,q)*xp*yq

            ! Extra contribution to H20(p) : P_p
            H20(p) = H20(p)                                            &
                  + cxp*cxp*V(q,p)*xq*yq                               &
                  + 2.0_pr*cxp*cyq*W(p,q)*yp*yq                        &
                  - cxq*cyq*V(p,q)*yp*yp

            ! Extra contribution to H11(p) : N_p
            H11(p) = H11(p)                                            &
                  - cxp*cyp*V(q,p)*xq*yq                               &
                  + cxp*cyq*W(p,q)*xp*yq                               &
                  - cxq*cyq*V(p,q)*xp*yp                               &
                  - cyp*cyq*W(p,q)*yp*yq

            ! H04(p,q) : P_p P_q  (raw, symmetrized later)
            H04(p,q) = cxp*cxq*W(p,q)*yp*yq - cxq*cxq*V(p,q)*yp*yp

            ! H31(p,q) : Pdag_p N_q
            H31(p,q) = cxq*cyp*W(p,q)*xp*xq                            &
                     - cxq*cyq*V(p,q)*xp*xp                            &
                     + cyp*cyp*V(q,p)*xq*yq                            &
                     - cyp*cyq*W(p,q)*xp*yq

            ! H13(p,q) : N_p P_q
            H13(p,q) = cxp*cxq*W(p,q)*xp*yq                            &
                     + cxp*cyp*V(q,p)*yq*yq                            &
                     - cxq*cxq*V(p,q)*xp*yp                            &
                     - cxq*cyp*W(p,q)*yp*yq

            ! H22(p,q) : N_p N_q
            H22(p,q) = 0.25_pr*cxp*cxq*W(p,q)*xp*xq                    &
                     - 0.5_pr*cxp*cyq*W(p,q)*xp*yq                     &
                     + cxq*cyq*V(p,q)*xp*yp                            &
                     + 0.25_pr*cyp*cyq*W(p,q)*yp*yq

         end do
      end do

      ! Enforce symmetric storage for Pdag Pdag and P P blocks
      ! to avoid double counting.
      do p = 1, NAO

         H40(p,p) = z0
         H04(p,p) = z0

         do q = p + 1, NAO

            tmp = 0.5_pr * ( H40(p,q) + H40(q,p) )
            H40(p,q) = tmp
            H40(q,p) = tmp

            tmp = 0.5_pr * ( H04(p,q) + H04(q,p) )
            H04(p,q) = tmp
            H04(q,p) = tmp

         end do
      end do

end subroutine Hub2PairComplex 