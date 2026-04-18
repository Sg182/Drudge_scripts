subroutine NpNq_integral(X, Y, NAO, &
                         N00, N20p, N20q, N11p, N11q, N02p, N02q, &
                         N40, N31pq, N31qp, N22NN, Nt22pq, Nt22qp, N13pq, N13qp, N04)
   use Precision
   implicit none

   integer, intent(in) :: NAO
   complex(kind=pr), intent(in)  :: X(NAO), Y(NAO)

   ! All outputs are functions of the external pair (p,q), so all are NAO x NAO
   complex(kind=pr), intent(out) :: N00(NAO,NAO)
   complex(kind=pr), intent(out) :: N20p(NAO,NAO), N20q(NAO,NAO)
   complex(kind=pr), intent(out) :: N11p(NAO,NAO), N11q(NAO,NAO)
   complex(kind=pr), intent(out) :: N02p(NAO,NAO), N02q(NAO,NAO)

   complex(kind=pr), intent(out) :: N40(NAO,NAO)
   complex(kind=pr), intent(out) :: N31pq(NAO,NAO), N31qp(NAO,NAO)
   complex(kind=pr), intent(out) :: N22NN(NAO,NAO)
   complex(kind=pr), intent(out) :: Nt22pq(NAO,NAO), Nt22qp(NAO,NAO)
   complex(kind=pr), intent(out) :: N13pq(NAO,NAO), N13qp(NAO,NAO)
   complex(kind=pr), intent(out) :: N04(NAO,NAO)

   integer :: p, q
   complex(kind=pr) :: up, uq, vp, vq
   complex(kind=pr) :: delta_pq
   complex(kind=pr), parameter :: zero = cmplx(0.0_pr, 0.0_pr, kind=pr)
   complex(kind=pr), parameter :: one  = cmplx(1.0_pr, 0.0_pr, kind=pr)

   do p = 1, NAO
      up = X(p)
      vp = Y(p)

      do q = 1, NAO
         uq = X(q)
         vq = Y(q)

         if (p == q) then
            delta_pq = one
         else
            delta_pq = zero
         end if

         !---------------------------------------------------------------
         ! Scalar part
         ! (4 δ_pq u_p^2 v_p^2 + 4 v_p^2 v_q^2)
         !---------------------------------------------------------------
         N00(p,q) = 4.0_pr*delta_pq*up**2*vp**2 + 4.0_pr*vp**2*vq**2

         !---------------------------------------------------------------
         ! One-body P parts
         !---------------------------------------------------------------
         ! P_p
         N20p(p,q) = 4.0_pr*delta_pq*up**3*vp - 4.0_pr*delta_pq*up*vp**3 &
                   + 4.0_pr*up*vp*vq**2

         ! P_q
         N20q(p,q) = 4.0_pr*vp**2*uq*vq

         !---------------------------------------------------------------
         ! One-body P^\dagger parts
         !---------------------------------------------------------------
         ! P^\dagger_p
         N02p(p,q) = 4.0_pr*delta_pq*up**3*vp - 4.0_pr*delta_pq*up*vp**3 &
                   + 4.0_pr*up*vp*vq**2

         ! P^\dagger_q
         N02q(p,q) = 4.0_pr*vp**2*uq*vq

         !---------------------------------------------------------------
         ! One-body N parts
         !---------------------------------------------------------------
         ! N_p
         N11p(p,q) = -4.0_pr*delta_pq*up**2*vp**2 + 2.0_pr*up**2*vq**2 &
                   - 2.0_pr*vp**2*vq**2

         ! N_q
         N11q(p,q) = 2.0_pr*uq**2*vp**2 - 2.0_pr*vp**2*vq**2

         !---------------------------------------------------------------
         ! Two-body NN
         !---------------------------------------------------------------
         N22NN(p,q) = up**2*uq**2 - up**2*vq**2 - uq**2*vp**2 + vp**2*vq**2

         !---------------------------------------------------------------
         ! Two-body PP
         !---------------------------------------------------------------
         N40(p,q) = 4.0_pr*up*uq*vp*vq

         !---------------------------------------------------------------
         ! Two-body P^\dagger P^\dagger
         !---------------------------------------------------------------
         N04(p,q) = 4.0_pr*up*uq*vp*vq

         !---------------------------------------------------------------
         ! Two-body P^\dagger P
         !---------------------------------------------------------------
         ! P^\dagger_p P_q
         Nt22pq(p,q) = 4.0_pr*up*uq*vp*vq

         ! P^\dagger_q P_p
         Nt22qp(p,q) = 4.0_pr*up*uq*vp*vq

         !---------------------------------------------------------------
         ! Mixed NP
         !---------------------------------------------------------------
         ! N_p P_q
         N31pq(p,q) = 2.0_pr*up**2*uq*vq - 2.0_pr*uq*vp**2*vq

         ! N_q P_p
         N31qp(p,q) = 2.0_pr*up*uq**2*vp - 2.0_pr*up*vp*vq**2

         !---------------------------------------------------------------
         ! Mixed P^\dagger N
         !---------------------------------------------------------------
         ! P^\dagger_p N_q
         N13pq(p,q) = 2.0_pr*up*uq**2*vp - 2.0_pr*up*vp*vq**2

         ! P^\dagger_q N_p
         N13qp(p,q) = 2.0_pr*up**2*uq*vq - 2.0_pr*uq*vp**2*vq

      end do
   end do

end subroutine NpNq_integral
