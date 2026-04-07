subroutine SpSq_integral(U, V, NAO, &
                        S00, S20p, S20q, S11p, S11q, S02p, S02q, &
                        S40, S31pq, S31qp, S22NN, St22pq, St22qp, S13pq, S13qp, S04)

      use Precision
      implicit none

      integer, intent(in) :: NAO
      complex(kind=pr), intent(in)  :: U(NAO), V(NAO)

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
      complex(kind=pr) :: cup, cuq, cvp, cvq
      complex(kind=pr) :: delta_pq

      complex(kind=pr), parameter :: zero    = cmplx(0.0_pr, 0.0_pr, kind=pr)
      complex(kind=pr), parameter :: one     = cmplx(1.0_pr, 0.0_pr, kind=pr)
      complex(kind=pr), parameter :: half    = cmplx(0.5_pr, 0.0_pr, kind=pr)
      complex(kind=pr), parameter :: quarter = cmplx(0.25_pr,0.0_pr, kind=pr)

      do p = 1, NAO
         up  = U(p)
         vp  = V(p)
         cup = conjg(up)
         cvp = conjg(vp)

         do q = 1, NAO
            uq  = U(q)
            vq  = V(q)
            cuq = conjg(uq)
            cvq = conjg(vq)

            if (p == q) then
               delta_pq = one
            else
               delta_pq = zero
            end if

            !---------------------------------------------------------------
            ! Scalar part
            !---------------------------------------------------------------
            S00(p,q) = cup*cvp*delta_pq*up*vp                                  &
                     + half*cup*cvp*uq*vq                                       &
                     + half*cuq*cvq*up*vp                                       &
                     + cvp*cvp*delta_pq*vp*vp                                   &
                     + cvp*cvq*vp*vq                                            &
                     - half*cvp*vp                                              &
                     - half*cvq*vq                                              &
                     + quarter

            !---------------------------------------------------------------
            ! One-body P^\dagger parts
            !---------------------------------------------------------------
            S02p(p,q) = cup*cvp*delta_pq*up*up                                  &
                      + half*cuq*cvq*up*up                                       &
                      - half*cvp*cvp*uq*vq                                       &
                      + cvp*cvq*up*vq                                            &
                      - half*cvp*up

            S02q(p,q) = half*cup*cvp*uq*uq                                      &
                      + cvp*cvp*delta_pq*up*vp                                   &
                      + cvp*cvq*uq*vp                                            &
                      - half*cvq*cvq*up*vp                                       &
                      - half*cvq*uq

            !---------------------------------------------------------------
            ! One-body P parts
            !---------------------------------------------------------------
            S20p(p,q) = cup*cup*delta_pq*up*vp                                  &
                      + half*cup*cup*uq*vq                                       &
                      + cup*cvq*vp*vq                                            &
                      - half*cup*vp                                              &
                      - half*cuq*cvq*vp*vp

            S20q(p,q) = cup*cvp*delta_pq*vp*vp                                  &
                      - half*cup*cvp*vq*vq                                       &
                      + half*cuq*cuq*up*vp                                       &
                      + cuq*cvp*vp*vq                                            &
                      - half*cuq*vq

            !---------------------------------------------------------------
            ! One-body N parts
            !---------------------------------------------------------------
            S11p(p,q) = -( cup*cvp*delta_pq*up*vp                               &
                        + half*cup*cvp*uq*vq                                     &
                        - half*cup*cvq*up*vq                                     &
                        + quarter*cup*up                                         &
                        + half*cuq*cvq*up*vp                                     &
                        + half*cvp*cvp*delta_pq*vp*vp                            &
                        + half*cvp*cvq*vp*vq                                     &
                        - quarter*cvp*vp )

            S11q(p,q) = -( half*cup*cvp*uq*vq                                   &
                        - half*cuq*cvp*uq*vp                                     &
                        + half*cuq*cvq*up*vp                                     &
                        + quarter*cuq*uq                                         &
                        + half*cvp*cvp*delta_pq*vp*vp                            &
                        + half*cvp*cvq*vp*vq                                     &
                        - quarter*cvq*vq )

            !---------------------------------------------------------------
            ! Two-body NN
            !---------------------------------------------------------------
            S22NN(p,q) = quarter*cup*cuq*up*uq                                  &
                       + half*cup*cvp*uq*vq                                      &
                       - quarter*cup*cvq*up*vq                                   &
                       - quarter*cuq*cvp*uq*vp                                   &
                       + half*cuq*cvq*up*vp                                      &
                       + quarter*cvp*cvq*vp*vq

            !---------------------------------------------------------------
            ! Two-body P^\dagger P^\dagger
            !---------------------------------------------------------------
            S04(p,q) = -( half*cvp*cvp*uq*uq                                    &
                       - cvp*cvq*up*uq                                           &
                       + half*cvq*cvq*up*up )

            !---------------------------------------------------------------
            ! Two-body P P
            !---------------------------------------------------------------
            S40(p,q) = -( half*cup*cup*vq*vq                                    &
                       - cup*cuq*vp*vq                                           &
                       + half*cuq*cuq*vp*vp )

            !---------------------------------------------------------------
            ! Two-body P^\dagger P
            ! St22pq  ~ P^\dagger_p P_q
            ! St22qp  ~ P^\dagger_q P_p
            !---------------------------------------------------------------
            St22pq(p,q) = half*cuq*cuq*up*up                                    &
                         + cuq*cvp*up*vq                                         &
                         + half*cvp*cvp*vq*vq

            St22qp(p,q) = half*cup*cup*uq*uq                                    &
                         + cup*cvq*uq*vp                                         &
                         + half*cvq*cvq*vp*vp

            !---------------------------------------------------------------
            ! Mixed N P
            ! S13pq ~ N_p P_q
            ! S13qp ~ N_q P_p
            !---------------------------------------------------------------
            S13pq(p,q) = half*cup*cuq*up*vq                                     &
                        + half*cup*cvp*vq*vq                                     &
                        - half*cuq*cuq*up*vp                                     &
                        - half*cuq*cvp*vp*vq

            S13qp(p,q) = -( half*cup*cup*uq*vq                                  &
                          - half*cup*cuq*uq*vp                                   &
                          + half*cup*cvq*vp*vq                                   &
                          - half*cuq*cvq*vp*vp )

            !---------------------------------------------------------------
            ! Mixed P^\dagger N
            ! S31pq ~ P^\dagger_p N_q
            ! S31qp ~ P^\dagger_q N_p
            !---------------------------------------------------------------
            S31pq(p,q) = half*cuq*cvp*up*uq                                     &
                        - half*cuq*cvq*up*up                                     &
                        + half*cvp*cvp*uq*vq                                     &
                        - half*cvp*cvq*up*vq

            S31qp(p,q) = -( half*cup*cvp*uq*uq                                  &
                          - half*cup*cvq*up*uq                                   &
                          + half*cvp*cvq*uq*vp                                   &
                          - half*cvq*cvq*up*vp )

            !---------------------------------------------------------------
            ! Enforce nilpotency convention on diagonal
            !---------------------------------------------------------------
            if (p == q) then
               S40(p,q) = zero
               S04(p,q) = zero
            end if

         end do
      end do

end subroutine SpSq_integral
