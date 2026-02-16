    Module CCRes_SFS 
    Use Precision
    Use Constants

    Contains


    Subroutine CCSDT_SFS(Ene,Res1,Res2,Res3,T1,T2,T3,NAO,  &
        H001,H100,H010,H101,H020,H200,H002,H110,H011, &
        H030,H111,H120,H210,H021,H012,H201,H102,H003,H300)   !Don't forget to add H000 with the energy
    Implicit None
    Integer,           Intent(In)    :: NAO 
    Complex (Kind=pr), Intent(In)    :: T1(NAO)
    Complex (Kind=pr), Intent(In)    :: T2(NAO,NAO)
    Complex (Kind=pr), Intent(In)    :: T3(NAO,NAO,NAO)
    Complex(Kind=pr), Intent(In)  :: H001(NAO), H100(NAO), H010(NAO)
    Complex(Kind=pr), Intent(In)  :: H101(NAO,NAO), H200(NAO,NAO), H002(NAO,NAO)
    Complex(Kind=pr), Intent(In)  :: H110(NAO,NAO), H011(NAO,NAO),H020(NAO,NAO)
    Complex(Kind=pr), Intent(In)  :: H030(NAO,NAO,NAO), H111(NAO,NAO,NAO), H120(NAO,NAO,NAO)
    Complex(Kind=pr), Intent(In)  :: H210(NAO,NAO,NAO), H021(NAO,NAO,NAO), H012(NAO,NAO,NAO)
    Complex(Kind=pr), Intent(In)  :: H003(NAO,NAO,NAO), H300(NAO,NAO,NAO)
    Complex(Kind=pr), Intent(In)  :: H201(NAO,NAO,NAO), H102(NAO,NAO,NAO)

    Complex (Kind=pr), Intent(Out)   :: Ene, Res1(NAO), Res2(NAO,NAO), Res3(NAO,NAO,NAO)
    Integer                          :: p, q, r, s, i, j, k, l
    
    
    complex(kind=pr) , dimension(:), allocatable :: tau0

    complex(kind=pr) , dimension(:, :), allocatable :: tau1

    complex(kind=pr) , dimension(:), allocatable :: tau2

    complex(kind=pr) , dimension(:, :, :), allocatable :: tau3

    complex(kind=pr) , dimension(:, :, :), allocatable :: tau4

    complex(kind=pr) , dimension(:, :), allocatable :: tau5

    complex(kind=pr) , dimension(:, :), allocatable :: tau6

    complex(kind=pr) , dimension(:, :), allocatable :: tau7

    complex(kind=pr) , dimension(:, :), allocatable :: tau8

    complex(kind=pr) , dimension(:, :), allocatable :: tau9

    complex(kind=pr) , dimension(:), allocatable :: tau10

    complex(kind=pr) , dimension(:, :), allocatable :: tau11

    complex(kind=pr) , dimension(:), allocatable :: tau12

    complex(kind=pr) , dimension(:), allocatable :: tau13

    complex(kind=pr) , dimension(:), allocatable :: tau14

    complex(kind=pr) , dimension(:), allocatable :: tau15

    complex(kind=pr) , dimension(:), allocatable :: tau16

    complex(kind=pr) , dimension(:), allocatable :: tau17

    complex(kind=pr) , dimension(:), allocatable :: tau18

    complex(kind=pr) , dimension(:), allocatable :: tau19

    complex(kind=pr) , dimension(:), allocatable :: tau20

    complex(kind=pr) , dimension(:), allocatable :: tau21

    complex(kind=pr) , dimension(:, :), allocatable :: tau22

    complex(kind=pr) , dimension(:, :), allocatable :: tau23

    complex(kind=pr) , dimension(:, :), allocatable :: tau24

    complex(kind=pr) , dimension(:, :), allocatable :: tau25

    complex(kind=pr) , dimension(:, :), allocatable :: tau26

    complex(kind=pr) , dimension(:, :), allocatable :: tau27

    complex(kind=pr) , dimension(:, :), allocatable :: tau28

    complex(kind=pr) , dimension(:, :), allocatable :: tau29

    complex(kind=pr) , dimension(:, :), allocatable :: tau30

    complex(kind=pr) , dimension(:, :), allocatable :: tau31

    complex(kind=pr) , dimension(:, :), allocatable :: tau32

    complex(kind=pr) , dimension(:, :), allocatable :: tau33

    complex(kind=pr) , dimension(:, :), allocatable :: tau34

    complex(kind=pr) , dimension(:, :), allocatable :: tau35

    complex(kind=pr) , dimension(:, :), allocatable :: tau36

    complex(kind=pr) , dimension(:, :), allocatable :: tau37

    complex(kind=pr) , dimension(:, :), allocatable :: tau38

    complex(kind=pr) , dimension(:, :), allocatable :: tau39

    complex(kind=pr) , dimension(:, :), allocatable :: tau40

    complex(kind=pr) , dimension(:, :), allocatable :: tau41

    complex(kind=pr) , dimension(:), allocatable :: tau42

    complex(kind=pr) , dimension(:), allocatable :: tau43

    complex(kind=pr) , dimension(:, :), allocatable :: tau44

    complex(kind=pr) , dimension(:, :), allocatable :: tau45

    complex(kind=pr) , dimension(:, :), allocatable :: tau46

    complex(kind=pr) , dimension(:, :), allocatable :: tau47

    complex(kind=pr) , dimension(:, :), allocatable :: tau48

    complex(kind=pr) , dimension(:, :, :), allocatable :: tau49

    complex(kind=pr) , dimension(:, :, :), allocatable :: tau50

    complex(kind=pr) , dimension(:, :), allocatable :: tau51

    complex(kind=pr) , dimension(:, :), allocatable :: tau52

    complex(kind=pr) , dimension(:, :), allocatable :: tau53

    complex(kind=pr) , dimension(:, :), allocatable :: tau54

    complex(kind=pr) , dimension(:, :), allocatable :: tau55

    complex(kind=pr) , dimension(:, :), allocatable :: tau56

    complex(kind=pr) , dimension(:, :), allocatable :: tau57

    complex(kind=pr) , dimension(:, :), allocatable :: tau58

    complex(kind=pr) , dimension(:, :), allocatable :: tau59

    complex(kind=pr) , dimension(:, :), allocatable :: tau60

    complex(kind=pr) , dimension(:, :), allocatable :: tau61

    complex(kind=pr) , dimension(:, :), allocatable :: tau62

    complex(kind=pr) , dimension(:), allocatable :: tau63

    complex(kind=pr) , dimension(:, :), allocatable :: tau64

    complex(kind=pr) , dimension(:, :), allocatable :: tau65

    complex(kind=pr) , dimension(:, :), allocatable :: tau66

    complex(kind=pr) , dimension(:, :), allocatable :: tau67

    complex(kind=pr) , dimension(:, :), allocatable :: tau68

    complex(kind=pr) , dimension(:), allocatable :: tau69

    complex(kind=pr) , dimension(:), allocatable :: tau70

    complex(kind=pr) , dimension(:, :), allocatable :: tau71

    complex(kind=pr) , dimension(:, :), allocatable :: tau72

    complex(kind=pr) , dimension(:, :), allocatable :: tau73

    complex(kind=pr) , dimension(:, :), allocatable :: tau74

    complex(kind=pr) , dimension(:, :), allocatable :: tau75

    complex(kind=pr) , dimension(:, :), allocatable :: tau76

    complex(kind=pr) , dimension(:, :, :), allocatable :: tau77

    complex(kind=pr) , dimension(:, :, :), allocatable :: tau78

    complex(kind=pr) , dimension(:, :, :), allocatable :: tau79

    complex(kind=pr) , dimension(:, :, :), allocatable :: tau80

    complex(kind=pr) , dimension(:, :, :), allocatable :: tau81

    complex(kind=pr) , dimension(:, :, :), allocatable :: tau82

    complex(kind=pr) , dimension(:, :, :), allocatable :: tau83

    complex(kind=pr) , dimension(:, :, :), allocatable :: tau84

    complex(kind=pr) , dimension(:, :, :), allocatable :: tau85

    complex(kind=pr) , dimension(:, :, :), allocatable :: tau86

    complex(kind=pr) , dimension(:, :, :), allocatable :: tau87

    complex(kind=pr) , dimension(:, :, :), allocatable :: tau88

    complex(kind=pr) , dimension(:, :, :), allocatable :: tau89

    complex(kind=pr) , dimension(:, :, :), allocatable :: tau90

    complex(kind=pr) , dimension(:, :, :), allocatable :: tau91

    complex(kind=pr) , dimension(:, :, :), allocatable :: tau92

    complex(kind=pr) , dimension(:, :, :), allocatable :: tau93

    complex(kind=pr) , dimension(:, :, :), allocatable :: tau94

    complex(kind=pr) , dimension(:, :, :), allocatable :: tau95

    complex(kind=pr) , dimension(:, :, :), allocatable :: tau96

    complex(kind=pr) , dimension(:, :, :), allocatable :: tau97

    complex(kind=pr) , dimension(:, :, :), allocatable :: tau98

    complex(kind=pr) , dimension(:, :, :), allocatable :: tau99

    complex(kind=pr) , dimension(:, :, :), allocatable :: tau100

    complex(kind=pr) , dimension(:, :, :), allocatable :: tau101

    complex(kind=pr) , dimension(:, :, :), allocatable :: tau102

    complex(kind=pr) , dimension(:, :, :), allocatable :: tau103

    complex(kind=pr) , dimension(:, :), allocatable :: tau104

    complex(kind=pr) , dimension(:, :, :), allocatable :: tau105

    complex(kind=pr) , dimension(:, :, :), allocatable :: tau106

    complex(kind=pr) , dimension(:, :), allocatable :: tau107

    complex(kind=pr) , dimension(:, :, :), allocatable :: tau108

    complex(kind=pr) , dimension(:, :, :), allocatable :: tau109

    complex(kind=pr) , dimension(:, :, :), allocatable :: tau110

    complex(kind=pr) , dimension(:, :), allocatable :: tau111

    complex(kind=pr) , dimension(:, :, :), allocatable :: tau112

    complex(kind=pr) , dimension(:, :), allocatable :: tau113

    complex(kind=pr) , dimension(:, :, :), allocatable :: tau114

    complex(kind=pr) , dimension(:, :, :), allocatable :: tau115

    complex(kind=pr) , dimension(:, :, :), allocatable :: tau116

    complex(kind=pr) , dimension(:, :, :), allocatable :: tau117

    complex(kind=pr) , dimension(:, :, :), allocatable :: tau118

    complex(kind=pr) , dimension(:, :, :), allocatable :: tau119

    complex(kind=pr) , dimension(:, :, :), allocatable :: tau120

    complex(kind=pr) , dimension(:, :, :), allocatable :: tau121

    complex(kind=pr) , dimension(:, :, :), allocatable :: tau122

    complex(kind=pr) , dimension(:, :, :), allocatable :: tau123

    complex(kind=pr) , dimension(:, :, :), allocatable :: tau124

    complex(kind=pr) , dimension(:, :, :), allocatable :: tau125

    complex(kind=pr) , dimension(:, :), allocatable :: tau126

    complex(kind=pr) , dimension(:, :), allocatable :: tau127

    complex(kind=pr) , dimension(:, :), allocatable :: tau128

    complex(kind=pr) , dimension(:, :, :), allocatable :: tau129

    complex(kind=pr) , dimension(:, :), allocatable :: tau130

    complex(kind=pr) , dimension(:, :, :), allocatable :: tau131

    complex(kind=pr) , dimension(:, :, :), allocatable :: tau132

    complex(kind=pr) , dimension(:, :, :), allocatable :: tau133

    complex(kind=pr) , dimension(:, :, :), allocatable :: tau134

    complex(kind=pr) , dimension(:, :, :), allocatable :: tau135

    complex(kind=pr) , dimension(:, :, :), allocatable :: tau136

    complex(kind=pr) , dimension(:, :, :), allocatable :: tau137

    complex(kind=pr) , dimension(:, :, :), allocatable :: tau138

    complex(kind=pr) , dimension(:, :), allocatable :: tau139

    complex(kind=pr) , dimension(:, :, :), allocatable :: tau140

    complex(kind=pr) , dimension(:, :, :), allocatable :: tau141

    complex(kind=pr) , dimension(:, :, :), allocatable :: tau142

    complex(kind=pr) , dimension(:, :, :), allocatable :: tau143

    complex(kind=pr) , dimension(:, :), allocatable :: tau144

    complex(kind=pr) , dimension(:, :), allocatable :: tau145

    complex(kind=pr) , dimension(:, :), allocatable :: tau146

    complex(kind=pr) , dimension(:, :), allocatable :: tau147

    complex(kind=pr) , dimension(:, :), allocatable :: tau148

    complex(kind=pr) , dimension(:), allocatable :: tau149

    complex(kind=pr) , dimension(:, :), allocatable :: tau150

    complex(kind=pr) , dimension(:, :, :), allocatable :: tau151

    complex(kind=pr) , dimension(:, :, :), allocatable :: tau152

    !$omp parallel default(shared)

    allocate(tau0(NAO))
    !$omp single
    tau0 = 0.0
    !$omp end single

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            tau0(p) = tau0(p) + ( &
                t1(q) * H002(p, q)&
            )
        end do
    end do
    !$omp end do

    allocate(tau2(NAO))
    !$omp single
    tau2 = 0.0
    !$omp end single

    !$omp do schedule(static)
    do p=1, NAO
    
        tau2(p) = tau2(p) + ( &
            tau0(p)&
        )
    
    end do
    !$omp end do

    allocate(tau19(NAO))
    !$omp single
    tau19 = 0.0
    !$omp end single

    !$omp do schedule(static)
    do p=1, NAO
    
        tau19(p) = tau19(p) + ( &
            2 * tau0(p)&
        )
    
    end do
    !$omp end do

    deallocate(tau0)

    allocate(tau1(NAO, NAO))
    !$omp single
    tau1 = 0.0
    !$omp end single

    !$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
    
            tau1(p, q) = tau1(p, q) + ( &
                t1(p) * t1(q)&
            )
    
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
    
            tau1(p, q) = tau1(p, q) + ( &
                3 * t2(q, p)&
            )
    
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do r=1, NAO
            do q=1, NAO
                tau2(p) = tau2(p) + ( &
                    tau1(q, r) * H003(q, p, r)&
                )
            end do
        end do
    end do
    !$omp end do

    deallocate(tau1)

    !$omp do schedule(static)
    do p=1, NAO
    
        tau2(p) = tau2(p) + ( &
            H001(p)&
        )
    
    end do
    !$omp end do

    !$omp single
    Ene = 0.0
    !$omp end single

    !$omp do schedule(static) reduction(+:Ene)
    
    do p=1, NAO
        Ene = Ene + ( &
            t1(p) * tau2(p)&
        )
    end do
    
    !$omp end do

    deallocate(tau2)

    allocate(tau3(NAO, NAO, NAO))
    !$omp single
    tau3 = 0.0
    !$omp end single

    !$omp do schedule(static)
    do r=1, NAO
        do q=1, NAO
            do p=1, NAO
    
                tau3(p, q, r) = tau3(p, q, r) + ( &
                    t3(r, p, q)&
                )
    
            end do
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do r=1, NAO
        do q=1, NAO
            do p=1, NAO
    
                tau3(p, q, r) = tau3(p, q, r) + ( &
                    2 * t1(r) * t2(q, p)&
                )
    
            end do
        end do
    end do
    !$omp end do

    !$omp single
    Res1 = 0.0
    !$omp end single

    !$omp do schedule(static)
    do p=1, NAO
        do r=1, NAO
            do q=1, NAO
                Res1(p) = Res1(p) + ( &
                    2 * H012(p, q, r) * tau3(q, p, r)&
                )
            end do
        end do
    end do
    !$omp end do

    deallocate(tau3)

    allocate(tau4(NAO, NAO, NAO))
    !$omp single
    tau4 = 0.0
    !$omp end single

    !$omp do schedule(static)
    do r=1, NAO
        do q=1, NAO
            do p=1, NAO
    
                tau4(p, q, r) = tau4(p, q, r) + ( &
                    t1(p) * t3(q, q, r)&
                )
    
            end do
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do r=1, NAO
        do q=1, NAO
            do p=1, NAO
    
                tau4(p, q, r) = tau4(p, q, r) + ( &
                    t2(q, p) * t2(q, r)&
                )
    
            end do
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do r=1, NAO
            do q=1, NAO
                Res1(p) = Res1(p) - ( &
                    6 * H003(q, p, r) * tau4(r, p, q)&
                )
            end do
        end do
    end do
    !$omp end do

    deallocate(tau4)

    allocate(tau5(NAO, NAO))
    !$omp single
    tau5 = 0.0
    !$omp end single

    !$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
    
            tau5(p, q) = tau5(p, q) + ( &
                H002(q, p)&
            )
    
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
    
            tau5(p, q) = tau5(p, q) + ( &
                2 * H012(q, q, p)&
            )
    
        end do
    end do
    !$omp end do

    allocate(tau17(NAO))
    !$omp single
    tau17 = 0.0
    !$omp end single

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            tau17(p) = tau17(p) + ( &
                t1(q) * tau5(q, p)&
            )
        end do
    end do
    !$omp end do

    allocate(tau18(NAO))
    !$omp single
    tau18 = 0.0
    !$omp end single

    !$omp do schedule(static)
    do p=1, NAO
    
        tau18(p) = tau18(p) + ( &
            2 * tau17(p)&
        )
    
    end do
    !$omp end do

    allocate(tau63(NAO))
    !$omp single
    tau63 = 0.0
    !$omp end single

    !$omp do schedule(static)
    do p=1, NAO
    
        tau63(p) = tau63(p) + ( &
            2 * tau17(p)&
        )
    
    end do
    !$omp end do

    deallocate(tau17)

    allocate(tau69(NAO))
    !$omp single
    tau69 = 0.0
    !$omp end single

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            tau69(p) = tau69(p) + ( &
                t2(q, p) * tau5(q, p)&
            )
        end do
    end do
    !$omp end do

    allocate(tau70(NAO))
    !$omp single
    tau70 = 0.0
    !$omp end single

    !$omp do schedule(static)
    do p=1, NAO
    
        tau70(p) = tau70(p) - ( &
            2 * tau69(p)&
        )
    
    end do
    !$omp end do

    deallocate(tau69)

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            Res1(p) = Res1(p) - ( &
                2 * tau5(q, p) * t3(p, p, q)&
            )
        end do
    end do
    !$omp end do

    deallocate(tau5)

    allocate(tau6(NAO, NAO))
    !$omp single
    tau6 = 0.0
    !$omp end single

    !$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
    
            tau6(p, q) = tau6(p, q) + ( &
                H011(p, q)&
            )
    
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
    
            tau6(p, q) = tau6(p, q) + ( &
                2 * H021(p, p, q)&
            )
    
        end do
    end do
    !$omp end do

    allocate(tau14(NAO))
    !$omp single
    tau14 = 0.0
    !$omp end single

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            tau14(p) = tau14(p) + ( &
                t1(q) * tau6(p, q)&
            )
        end do
    end do
    !$omp end do

    allocate(tau15(NAO))
    !$omp single
    tau15 = 0.0
    !$omp end single

    !$omp do schedule(static)
    do p=1, NAO
    
        tau15(p) = tau15(p) + ( &
            tau14(p)&
        )
    
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
    
        tau70(p) = tau70(p) + ( &
            tau14(p)&
        )
    
    end do
    !$omp end do

    allocate(tau149(NAO))
    !$omp single
    tau149 = 0.0
    !$omp end single

    !$omp do schedule(static)
    do p=1, NAO
    
        tau149(p) = tau149(p) - ( &
            tau14(p)&
        )
    
    end do
    !$omp end do

    deallocate(tau14)

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            Res1(p) = Res1(p) + ( &
                2 * t2(q, p) * tau6(p, q)&
            )
        end do
    end do
    !$omp end do

    deallocate(tau6)

    allocate(tau7(NAO, NAO))
    !$omp single
    tau7 = 0.0
    !$omp end single

    !$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
    
            tau7(p, q) = tau7(p, q) + ( &
                t2(q, p)&
            )
    
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
    
            tau7(p, q) = tau7(p, q) + ( &
                t1(p) * t1(q)&
            )
    
        end do
    end do
    !$omp end do

    allocate(tau13(NAO))
    !$omp single
    tau13 = 0.0
    !$omp end single

    !$omp do schedule(static)
    do p=1, NAO
        do r=1, NAO
            do q=1, NAO
                tau13(p) = tau13(p) + ( &
                    tau7(q, r) * H012(p, q, r)&
                )
            end do
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
    
        tau15(p) = tau15(p) + ( &
            tau13(p)&
        )
    
    end do
    !$omp end do

    allocate(tau43(NAO))
    !$omp single
    tau43 = 0.0
    !$omp end single

    !$omp do schedule(static)
    do p=1, NAO
    
        tau43(p) = tau43(p) - ( &
            tau13(p)&
        )
    
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
    
        tau149(p) = tau149(p) - ( &
            tau13(p)&
        )
    
    end do
    !$omp end do

    deallocate(tau13)

    allocate(tau16(NAO))
    !$omp single
    tau16 = 0.0
    !$omp end single

    !$omp do schedule(static)
    do p=1, NAO
        do r=1, NAO
            do q=1, NAO
                tau16(p) = tau16(p) + ( &
                    tau7(q, r) * H003(q, p, r)&
                )
            end do
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
    
        tau18(p) = tau18(p) + ( &
            3 * tau16(p)&
        )
    
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
    
        tau19(p) = tau19(p) + ( &
            3 * tau16(p)&
        )
    
    end do
    !$omp end do

    deallocate(tau16)

    allocate(tau102(NAO, NAO, NAO))
    !$omp single
    tau102 = 0.0
    !$omp end single

    !$omp do schedule(static)
    do r=1, NAO
        do q=1, NAO
            do p=1, NAO
    
                tau102(p, q, r) = tau102(p, q, r) + ( &
                    t1(p) * tau7(r, q)&
                )
    
            end do
        end do
    end do
    !$omp end do

    allocate(tau109(NAO, NAO, NAO))
    !$omp single
    tau109 = 0.0
    !$omp end single

    !$omp do schedule(static)
    do r=1, NAO
        do q=1, NAO
            do p=1, NAO
    
                tau109(p, q, r) = tau109(p, q, r) + ( &
                    t1(q) * tau7(r, p)&
                )
    
            end do
        end do
    end do
    !$omp end do

    allocate(tau117(NAO, NAO, NAO))
    !$omp single
    tau117 = 0.0
    !$omp end single

    !$omp do schedule(static)
    do r=1, NAO
        do q=1, NAO
            do p=1, NAO
    
                tau117(p, q, r) = tau117(p, q, r) + ( &
                    t1(p)**2 * tau7(r, q)&
                )
    
            end do
        end do
    end do
    !$omp end do

    allocate(tau119(NAO, NAO, NAO))
    !$omp single
    tau119 = 0.0
    !$omp end single

    !$omp do schedule(static)
    do r=1, NAO
        do q=1, NAO
            do p=1, NAO
    
                tau119(p, q, r) = tau119(p, q, r) + ( &
                    t1(p)**2 * tau7(r, q)&
                )
    
            end do
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do r=1, NAO
            do q=1, NAO
                Res1(p) = Res1(p) + ( &
                    tau7(q, r) * H102(p, q, r)&
                )
            end do
        end do
    end do
    !$omp end do

    allocate(tau8(NAO, NAO))
    !$omp single
    tau8 = 0.0
    !$omp end single

    !$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
            do r=1, NAO
                tau8(p, q) = tau8(p, q) + ( &
                    t1(r) * H003(p, r, q)&
                )
            end do
        end do
    end do
    !$omp end do

    allocate(tau9(NAO, NAO))
    !$omp single
    tau9 = 0.0
    !$omp end single

    !$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
    
            tau9(p, q) = tau9(p, q) + ( &
                3 * tau8(q, p)&
            )
    
        end do
    end do
    !$omp end do

    allocate(tau11(NAO, NAO))
    !$omp single
    tau11 = 0.0
    !$omp end single

    !$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
    
            tau11(p, q) = tau11(p, q) + ( &
                3 * tau8(q, p)&
            )
    
        end do
    end do
    !$omp end do

    allocate(tau57(NAO, NAO))
    !$omp single
    tau57 = 0.0
    !$omp end single

    !$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
    
            tau57(p, q) = tau57(p, q) + ( &
                3 * tau8(q, p)&
            )
    
        end do
    end do
    !$omp end do

    !$omp single
    Res2 = 0.0
    !$omp end single

    !$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
    
            Res2(p, q) = Res2(p, q) + ( &
                12 * t2(q, p)**2 * tau8(q, p)&
            )
    
        end do
    end do
    !$omp end do

    deallocate(tau8)

    !$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
    
            tau9(p, q) = tau9(p, q) + ( &
                H002(q, p)&
            )
    
        end do
    end do
    !$omp end do

    allocate(tau72(NAO, NAO))
    !$omp single
    tau72 = 0.0
    !$omp end single

    !$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
            do r=1, NAO
                tau72(p, q) = tau72(p, q) + ( &
                    t2(r, p) * tau9(r, q)&
                )
            end do
        end do
    end do
    !$omp end do

    allocate(tau73(NAO, NAO))
    !$omp single
    tau73 = 0.0
    !$omp end single

    !$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
    
            tau73(p, q) = tau73(p, q) + ( &
                2 * tau72(q, p)&
            )
    
        end do
    end do
    !$omp end do

    allocate(tau76(NAO, NAO))
    !$omp single
    tau76 = 0.0
    !$omp end single

    !$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
    
            tau76(p, q) = tau76(p, q) + ( &
                2 * tau72(q, p)&
            )
    
        end do
    end do
    !$omp end do

    deallocate(tau72)

    !$omp do schedule(static)
    do p=1, NAO
        do r=1, NAO
            do q=1, NAO
                Res1(p) = Res1(p) + ( &
                    tau9(q, r) * t3(q, p, r)&
                )
            end do
        end do
    end do
    !$omp end do

    deallocate(tau9)

    allocate(tau10(NAO))
    !$omp single
    tau10 = 0.0
    !$omp end single

    !$omp do schedule(static)
    do p=1, NAO
        do r=1, NAO
            do q=1, NAO
                tau10(p) = tau10(p) + ( &
                    H003(p, q, r) * t3(p, q, r)&
                )
            end do
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
    
        tau15(p) = tau15(p) - ( &
            3 * tau10(p)&
        )
    
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
    
        tau43(p) = tau43(p) + ( &
            3 * tau10(p)&
        )
    
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
    
        tau149(p) = tau149(p) + ( &
            3 * tau10(p)&
        )
    
    end do
    !$omp end do

    deallocate(tau10)

    !$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
    
            tau11(p, q) = tau11(p, q) + ( &
                H002(q, p)&
            )
    
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
    
            tau11(p, q) = tau11(p, q) + ( &
                2 * H012(q, q, p)&
            )
    
        end do
    end do
    !$omp end do

    allocate(tau12(NAO))
    !$omp single
    tau12 = 0.0
    !$omp end single

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            tau12(p) = tau12(p) + ( &
                t2(q, p) * tau11(q, p)&
            )
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
    
        tau15(p) = tau15(p) - ( &
            2 * tau12(p)&
        )
    
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
    
        tau149(p) = tau149(p) + ( &
            2 * tau12(p)&
        )
    
    end do
    !$omp end do

    deallocate(tau12)

    allocate(tau28(NAO, NAO))
    !$omp single
    tau28 = 0.0
    !$omp end single

    !$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
            do r=1, NAO
                tau28(p, q) = tau28(p, q) + ( &
                    tau11(r, p) * t3(r, p, q)&
                )
            end do
        end do
    end do
    !$omp end do

    allocate(tau33(NAO, NAO))
    !$omp single
    tau33 = 0.0
    !$omp end single

    !$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
    
            tau33(p, q) = tau33(p, q) + ( &
                2 * tau28(p, q)&
            )
    
        end do
    end do
    !$omp end do

    allocate(tau147(NAO, NAO))
    !$omp single
    tau147 = 0.0
    !$omp end single

    !$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
    
            tau147(p, q) = tau147(p, q) + ( &
                2 * tau28(p, q)&
            )
    
        end do
    end do
    !$omp end do

    allocate(tau148(NAO, NAO))
    !$omp single
    tau148 = 0.0
    !$omp end single

    !$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
    
            tau148(p, q) = tau148(p, q) + ( &
                2 * tau28(p, q)&
            )
    
        end do
    end do
    !$omp end do

    deallocate(tau28)

    allocate(tau36(NAO, NAO))
    !$omp single
    tau36 = 0.0
    !$omp end single

    !$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
            do r=1, NAO
                tau36(p, q) = tau36(p, q) + ( &
                    t2(r, p) * tau11(r, q)&
                )
            end do
        end do
    end do
    !$omp end do

    allocate(tau37(NAO, NAO))
    !$omp single
    tau37 = 0.0
    !$omp end single

    !$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
    
            tau37(p, q) = tau37(p, q) + ( &
                2 * tau36(q, p)&
            )
    
        end do
    end do
    !$omp end do

    allocate(tau145(NAO, NAO))
    !$omp single
    tau145 = 0.0
    !$omp end single

    !$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
    
            tau145(p, q) = tau145(p, q) + ( &
                2 * tau36(p, q)&
            )
    
        end do
    end do
    !$omp end do

    allocate(tau150(NAO, NAO))
    !$omp single
    tau150 = 0.0
    !$omp end single

    !$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
    
            tau150(p, q) = tau150(p, q) + ( &
                2 * tau36(q, p)&
            )
    
        end do
    end do
    !$omp end do

    deallocate(tau36)

    allocate(tau83(NAO, NAO, NAO))
    !$omp single
    tau83 = 0.0
    !$omp end single

    !$omp do schedule(static)
    do r=1, NAO
        do q=1, NAO
            do p=1, NAO
                do s=1, NAO
                    tau83(p, q, r) = tau83(p, q, r) + ( &
                        tau11(s, r) * t3(s, p, q)&
                    )
                end do
            end do
        end do
    end do
    !$omp end do

    allocate(tau84(NAO, NAO, NAO))
    !$omp single
    tau84 = 0.0
    !$omp end single

    !$omp do schedule(static)
    do r=1, NAO
        do q=1, NAO
            do p=1, NAO
    
                tau84(p, q, r) = tau84(p, q, r) + ( &
                    tau83(q, r, p)&
                )
    
            end do
        end do
    end do
    !$omp end do

    deallocate(tau83)

    allocate(tau144(NAO, NAO))
    !$omp single
    tau144 = 0.0
    !$omp end single

    !$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
            do r=1, NAO
                tau144(p, q) = tau144(p, q) + ( &
                    tau11(r, q) * t3(r, p, q)&
                )
            end do
        end do
    end do
    !$omp end do

    deallocate(tau11)

    allocate(tau146(NAO, NAO))
    !$omp single
    tau146 = 0.0
    !$omp end single

    !$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
    
            tau146(p, q) = tau146(p, q) + ( &
                2 * tau144(p, q)&
            )
    
        end do
    end do
    !$omp end do

    deallocate(tau144)

    !$omp do schedule(static)
    do p=1, NAO
    
        tau15(p) = tau15(p) + ( &
            H010(p)&
        )
    
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
    
        tau15(p) = tau15(p) + ( &
            2 * H020(p, p)&
        )
    
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
    
        tau15(p) = tau15(p) + ( &
            4 * H030(p, p, p)&
        )
    
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
    
        Res1(p) = Res1(p) + ( &
            2 * t1(p) * tau15(p)&
        )
    
    end do
    !$omp end do

    deallocate(tau15)

    !$omp do schedule(static)
    do p=1, NAO
    
        tau18(p) = tau18(p) + ( &
            H001(p)&
        )
    
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
    
        tau18(p) = tau18(p) + ( &
            2 * H011(p, p)&
        )
    
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
    
        tau18(p) = tau18(p) + ( &
            4 * H021(p, p, p)&
        )
    
    end do
    !$omp end do

    !$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
    
            tau146(p, q) = tau146(p, q) + ( &
                tau18(q) * t2(q, p)&
            )
    
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
    
            tau147(p, q) = tau147(p, q) + ( &
                tau18(p) * t2(q, p)&
            )
    
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
    
        tau149(p) = tau149(p) + ( &
            t1(p) * tau18(p)&
        )
    
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
    
        Res1(p) = Res1(p) - ( &
            t1(p)**2 * tau18(p)&
        )
    
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
    
        tau19(p) = tau19(p) + ( &
            H001(p)&
        )
    
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            Res1(p) = Res1(p) + ( &
                tau19(q) * t2(q, p)&
            )
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
            do r=1, NAO
                Res2(p, q) = Res2(p, q) + ( &
                    tau19(r) * t3(r, p, q)&
                )
            end do
        end do
    end do
    !$omp end do

    deallocate(tau19)

    allocate(tau20(NAO))
    !$omp single
    tau20 = 0.0
    !$omp end single

    !$omp do schedule(static)
    do p=1, NAO
        do r=1, NAO
            do q=1, NAO
                tau20(p) = tau20(p) + ( &
                    t2(r, q) * H003(r, p, q)&
                )
            end do
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
    
            Res2(p, q) = Res2(p, q) - ( &
                6 * t1(p) * tau20(p) * t2(q, p)&
            )
    
        end do
    end do
    !$omp end do

    deallocate(tau20)

    allocate(tau21(NAO))
    !$omp single
    tau21 = 0.0
    !$omp end single

    !$omp do schedule(static)
    do p=1, NAO
        do r=1, NAO
            do q=1, NAO
                tau21(p) = tau21(p) + ( &
                    t1(q) * t1(r) * H003(p, q, r)&
                )
            end do
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
    
            Res2(p, q) = Res2(p, q) - ( &
                6 * t1(p) * tau21(p) * t2(q, p)&
            )
    
        end do
    end do
    !$omp end do

    deallocate(tau21)

    allocate(tau22(NAO, NAO))
    !$omp single
    tau22 = 0.0
    !$omp end single

    !$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
            do r=1, NAO
                tau22(p, q) = tau22(p, q) + ( &
                    H102(p, q, r) * t3(q, q, r)&
                )
            end do
        end do
    end do
    !$omp end do

    allocate(tau52(NAO, NAO))
    !$omp single
    tau52 = 0.0
    !$omp end single

    !$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
    
            tau52(p, q) = tau52(p, q) + ( &
                2 * tau22(p, q)&
            )
    
        end do
    end do
    !$omp end do

    deallocate(tau22)

    allocate(tau23(NAO, NAO))
    !$omp single
    tau23 = 0.0
    !$omp end single

    !$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
            do r=1, NAO
                tau23(p, q) = tau23(p, q) + ( &
                    t1(r) * H201(p, q, r)&
                )
            end do
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
    
            tau52(p, q) = tau52(p, q) - ( &
                tau23(p, q)&
            )
    
        end do
    end do
    !$omp end do

    deallocate(tau23)

    allocate(tau24(NAO, NAO))
    !$omp single
    tau24 = 0.0
    !$omp end single

    !$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
            do r=1, NAO
                tau24(p, q) = tau24(p, q) + ( &
                    t2(q, r) * H111(p, q, r)&
                )
            end do
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
    
            tau52(p, q) = tau52(p, q) - ( &
                2 * tau24(p, q)&
            )
    
        end do
    end do
    !$omp end do

    deallocate(tau24)

    allocate(tau25(NAO, NAO))
    !$omp single
    tau25 = 0.0
    !$omp end single

    !$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
            do r=1, NAO
                tau25(p, q) = tau25(p, q) + ( &
                    H012(p, q, r) * t3(q, q, r)&
                )
            end do
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
    
            tau33(p, q) = tau33(p, q) + ( &
                2 * tau25(p, q)&
            )
    
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
    
            tau146(p, q) = tau146(p, q) + ( &
                2 * tau25(q, p)&
            )
    
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
    
            tau147(p, q) = tau147(p, q) + ( &
                2 * tau25(p, q)&
            )
    
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
    
            tau148(p, q) = tau148(p, q) + ( &
                2 * tau25(p, q)&
            )
    
        end do
    end do
    !$omp end do

    deallocate(tau25)

    allocate(tau26(NAO, NAO))
    !$omp single
    tau26 = 0.0
    !$omp end single

    !$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
            do r=1, NAO
                tau26(p, q) = tau26(p, q) + ( &
                    t2(p, r) * H021(p, q, r)&
                )
            end do
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
    
            tau33(p, q) = tau33(p, q) - ( &
                4 * tau26(q, p)&
            )
    
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
    
            tau146(p, q) = tau146(p, q) - ( &
                4 * tau26(p, q)&
            )
    
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
    
            tau147(p, q) = tau147(p, q) - ( &
                4 * tau26(q, p)&
            )
    
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
    
            tau148(p, q) = tau148(p, q) - ( &
                4 * tau26(q, p)&
            )
    
        end do
    end do
    !$omp end do

    deallocate(tau26)

    allocate(tau27(NAO, NAO))
    !$omp single
    tau27 = 0.0
    !$omp end single

    !$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
            do s=1, NAO
                do r=1, NAO
                    tau27(p, q) = tau27(p, q) + ( &
                        H012(p, s, r) * t3(q, r, s)&
                    )
                end do
            end do
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
    
            tau33(p, q) = tau33(p, q) - ( &
                tau27(p, q)&
            )
    
        end do
    end do
    !$omp end do

    allocate(tau100(NAO, NAO, NAO))
    !$omp single
    tau100 = 0.0
    !$omp end single

    !$omp do schedule(static)
    do r=1, NAO
        do q=1, NAO
            do p=1, NAO
    
                tau100(p, q, r) = tau100(p, q, r) - ( &
                    t2(q, p) * tau27(p, r)&
                )
    
            end do
        end do
    end do
    !$omp end do

    deallocate(tau27)

    allocate(tau29(NAO, NAO))
    !$omp single
    tau29 = 0.0
    !$omp end single

    !$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
            do r=1, NAO
                tau29(p, q) = tau29(p, q) + ( &
                    t1(r) * H012(p, q, r)&
                )
            end do
        end do
    end do
    !$omp end do

    allocate(tau31(NAO, NAO))
    !$omp single
    tau31 = 0.0
    !$omp end single

    !$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
    
            tau31(p, q) = tau31(p, q) + ( &
                2 * tau29(p, q)&
            )
    
        end do
    end do
    !$omp end do

    allocate(tau56(NAO, NAO))
    !$omp single
    tau56 = 0.0
    !$omp end single

    !$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
    
            tau56(p, q) = tau56(p, q) - ( &
                t1(q) * tau29(p, q)&
            )
    
        end do
    end do
    !$omp end do

    allocate(tau61(NAO, NAO))
    !$omp single
    tau61 = 0.0
    !$omp end single

    !$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
    
            tau61(p, q) = tau61(p, q) - ( &
                2 * tau29(p, q)&
            )
    
        end do
    end do
    !$omp end do

    allocate(tau62(NAO, NAO))
    !$omp single
    tau62 = 0.0
    !$omp end single

    !$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
    
            tau62(p, q) = tau62(p, q) + ( &
                2 * tau29(p, q)&
            )
    
        end do
    end do
    !$omp end do

    allocate(tau67(NAO, NAO))
    !$omp single
    tau67 = 0.0
    !$omp end single

    !$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
    
            tau67(p, q) = tau67(p, q) - ( &
                tau29(p, q)&
            )
    
        end do
    end do
    !$omp end do

    deallocate(tau29)

    allocate(tau30(NAO, NAO))
    !$omp single
    tau30 = 0.0
    !$omp end single

    !$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
            do r=1, NAO
                tau30(p, q) = tau30(p, q) + ( &
                    t2(p, r) * H003(p, r, q)&
                )
            end do
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
    
            tau31(p, q) = tau31(p, q) - ( &
                6 * tau30(p, q)&
            )
    
        end do
    end do
    !$omp end do

    allocate(tau42(NAO))
    !$omp single
    tau42 = 0.0
    !$omp end single

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            tau42(p) = tau42(p) + ( &
                t1(q) * tau30(p, q)&
            )
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
    
        tau43(p) = tau43(p) + ( &
            6 * tau42(p)&
        )
    
    end do
    !$omp end do

    deallocate(tau42)

    !$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
    
            tau52(p, q) = tau52(p, q) + ( &
                2 * tau43(p) * t2(q, p)&
            )
    
        end do
    end do
    !$omp end do

    deallocate(tau43)

    !$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
    
            tau56(p, q) = tau56(p, q) + ( &
                3 * t1(p) * tau30(q, p)&
            )
    
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
    
            tau61(p, q) = tau61(p, q) + ( &
                6 * tau30(p, q)&
            )
    
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
    
            tau62(p, q) = tau62(p, q) - ( &
                6 * tau30(p, q)&
            )
    
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
    
            tau67(p, q) = tau67(p, q) + ( &
                3 * tau30(p, q)&
            )
    
        end do
    end do
    !$omp end do

    deallocate(tau30)

    !$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
    
            Res2(p, q) = Res2(p, q) + ( &
                4 * t1(q)**2 * t1(p) * tau67(p, q)&
            )
    
        end do
    end do
    !$omp end do

    deallocate(tau67)

    !$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
    
            tau31(p, q) = tau31(p, q) + ( &
                H011(p, q)&
            )
    
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
    
            tau31(p, q) = tau31(p, q) + ( &
                2 * H021(p, p, q)&
            )
    
        end do
    end do
    !$omp end do

    allocate(tau32(NAO, NAO))
    !$omp single
    tau32 = 0.0
    !$omp end single

    !$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
            do r=1, NAO
                tau32(p, q) = tau32(p, q) + ( &
                    t2(r, p) * tau31(q, r)&
                )
            end do
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
    
            tau33(p, q) = tau33(p, q) - ( &
                tau32(q, p)&
            )
    
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
    
            tau52(p, q) = tau52(p, q) + ( &
                2 * t1(p) * tau33(p, q)&
            )
    
        end do
    end do
    !$omp end do

    deallocate(tau33)

    !$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
    
            tau146(p, q) = tau146(p, q) - ( &
                tau32(p, q)&
            )
    
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
    
            tau147(p, q) = tau147(p, q) - ( &
                tau32(q, p)&
            )
    
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
    
            tau148(p, q) = tau148(p, q) - ( &
                tau32(q, p)&
            )
    
        end do
    end do
    !$omp end do

    deallocate(tau32)

    allocate(tau80(NAO, NAO, NAO))
    !$omp single
    tau80 = 0.0
    !$omp end single

    !$omp do schedule(static)
    do r=1, NAO
        do q=1, NAO
            do p=1, NAO
                do s=1, NAO
                    tau80(p, q, r) = tau80(p, q, r) + ( &
                        tau31(r, s) * t3(s, p, q)&
                    )
                end do
            end do
        end do
    end do
    !$omp end do

    allocate(tau81(NAO, NAO, NAO))
    !$omp single
    tau81 = 0.0
    !$omp end single

    !$omp do schedule(static)
    do r=1, NAO
        do q=1, NAO
            do p=1, NAO
    
                tau81(p, q, r) = tau81(p, q, r) + ( &
                    tau80(q, r, p)&
                )
    
            end do
        end do
    end do
    !$omp end do

    deallocate(tau80)

    !$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
            do r=1, NAO
                Res2(p, q) = Res2(p, q) + ( &
                    2 * tau31(p, r) * t3(r, p, q)&
                )
            end do
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
            do r=1, NAO
                Res2(p, q) = Res2(p, q) + ( &
                    2 * tau31(q, r) * t3(r, p, q)&
                )
            end do
        end do
    end do
    !$omp end do

    deallocate(tau31)

    allocate(tau34(NAO, NAO))
    !$omp single
    tau34 = 0.0
    !$omp end single

    !$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
            do r=1, NAO
                tau34(p, q) = tau34(p, q) + ( &
                    H003(p, q, r) * t3(p, p, r)&
                )
            end do
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
    
            tau37(p, q) = tau37(p, q) - ( &
                6 * tau34(q, p)&
            )
    
        end do
    end do
    !$omp end do

    allocate(tau40(NAO, NAO))
    !$omp single
    tau40 = 0.0
    !$omp end single

    !$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
    
            tau40(p, q) = tau40(p, q) + ( &
                3 * tau34(p, q)&
            )
    
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
    
            tau76(p, q) = tau76(p, q) - ( &
                6 * tau34(q, p)&
            )
    
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
    
            tau145(p, q) = tau145(p, q) - ( &
                6 * tau34(p, q)&
            )
    
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
    
            tau150(p, q) = tau150(p, q) - ( &
                6 * tau34(q, p)&
            )
    
        end do
    end do
    !$omp end do

    deallocate(tau34)

    allocate(tau35(NAO, NAO))
    !$omp single
    tau35 = 0.0
    !$omp end single

    !$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
            do s=1, NAO
                do r=1, NAO
                    tau35(p, q) = tau35(p, q) + ( &
                        H003(s, p, r) * t3(q, r, s)&
                    )
                end do
            end do
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
    
            tau37(p, q) = tau37(p, q) + ( &
                3 * tau35(p, q)&
            )
    
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
    
            tau52(p, q) = tau52(p, q) + ( &
                t1(p)**2 * tau37(p, q)&
            )
    
        end do
    end do
    !$omp end do

    deallocate(tau37)

    allocate(tau89(NAO, NAO, NAO))
    !$omp single
    tau89 = 0.0
    !$omp end single

    !$omp do schedule(static)
    do r=1, NAO
        do q=1, NAO
            do p=1, NAO
                do s=1, NAO
                    tau89(p, q, r) = tau89(p, q, r) + ( &
                        tau35(s, r) * t3(p, s, q)&
                    )
                end do
            end do
        end do
    end do
    !$omp end do

    allocate(tau90(NAO, NAO, NAO))
    !$omp single
    tau90 = 0.0
    !$omp end single

    !$omp do schedule(static)
    do r=1, NAO
        do q=1, NAO
            do p=1, NAO
    
                tau90(p, q, r) = tau90(p, q, r) + ( &
                    3 * tau89(q, p, r)&
                )
    
            end do
        end do
    end do
    !$omp end do

    deallocate(tau89)

    !$omp do schedule(static)
    do r=1, NAO
        do q=1, NAO
            do p=1, NAO
    
                tau90(p, q, r) = tau90(p, q, r) - ( &
                    3 * tau35(q, r) * t3(q, q, p)&
                )
    
            end do
        end do
    end do
    !$omp end do

    allocate(tau94(NAO, NAO, NAO))
    !$omp single
    tau94 = 0.0
    !$omp end single

    !$omp do schedule(static)
    do r=1, NAO
        do q=1, NAO
            do p=1, NAO
    
                tau94(p, q, r) = tau94(p, q, r) + ( &
                    3 * t2(p, q) * tau35(q, r)&
                )
    
            end do
        end do
    end do
    !$omp end do

    deallocate(tau35)

    allocate(tau38(NAO, NAO))
    !$omp single
    tau38 = 0.0
    !$omp end single

    !$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
            do r=1, NAO
                tau38(p, q) = tau38(p, q) + ( &
                    t1(r) * H102(p, q, r)&
                )
            end do
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
    
            tau40(p, q) = tau40(p, q) - ( &
                tau38(p, q)&
            )
    
        end do
    end do
    !$omp end do

    allocate(tau48(NAO, NAO))
    !$omp single
    tau48 = 0.0
    !$omp end single

    !$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
    
            tau48(p, q) = tau48(p, q) + ( &
                tau38(p, q)&
            )
    
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
    
            tau76(p, q) = tau76(p, q) + ( &
                2 * tau38(q, p)&
            )
    
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
    
            tau145(p, q) = tau145(p, q) + ( &
                2 * tau38(p, q)&
            )
    
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
    
            tau150(p, q) = tau150(p, q) + ( &
                2 * tau38(q, p)&
            )
    
        end do
    end do
    !$omp end do

    deallocate(tau38)

    allocate(tau39(NAO, NAO))
    !$omp single
    tau39 = 0.0
    !$omp end single

    !$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
            do r=1, NAO
                tau39(p, q) = tau39(p, q) + ( &
                    t2(p, r) * H012(p, r, q)&
                )
            end do
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
    
            tau40(p, q) = tau40(p, q) - ( &
                2 * tau39(p, q)&
            )
    
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
    
            tau48(p, q) = tau48(p, q) + ( &
                2 * tau39(p, q)&
            )
    
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
    
            tau52(p, q) = tau52(p, q) + ( &
                2 * t1(q)**2 * tau48(p, q)&
            )
    
        end do
    end do
    !$omp end do

    deallocate(tau48)

    !$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
    
            tau76(p, q) = tau76(p, q) + ( &
                4 * tau39(q, p)&
            )
    
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
    
            tau145(p, q) = tau145(p, q) + ( &
                4 * tau39(p, q)&
            )
    
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
    
            tau150(p, q) = tau150(p, q) + ( &
                4 * tau39(q, p)&
            )
    
        end do
    end do
    !$omp end do

    deallocate(tau39)

    !$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
    
            tau40(p, q) = tau40(p, q) + ( &
                t1(q) * H102(p, q, q)&
            )
    
        end do
    end do
    !$omp end do

    allocate(tau41(NAO, NAO))
    !$omp single
    tau41 = 0.0
    !$omp end single

    !$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
            do r=1, NAO
                tau41(p, q) = tau41(p, q) + ( &
                    t2(r, p) * tau40(q, r)&
                )
            end do
        end do
    end do
    !$omp end do

    deallocate(tau40)

    !$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
    
            tau52(p, q) = tau52(p, q) + ( &
                2 * tau41(q, p)&
            )
    
        end do
    end do
    !$omp end do

    deallocate(tau41)

    allocate(tau44(NAO, NAO))
    !$omp single
    tau44 = 0.0
    !$omp end single

    !$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
            do r=1, NAO
                tau44(p, q) = tau44(p, q) + ( &
                    H012(p, q, r) * t3(p, r, q)&
                )
            end do
        end do
    end do
    !$omp end do

    allocate(tau47(NAO, NAO))
    !$omp single
    tau47 = 0.0
    !$omp end single

    !$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
    
            tau47(p, q) = tau47(p, q) + ( &
                4 * tau44(p, q)&
            )
    
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
    
            tau146(p, q) = tau146(p, q) + ( &
                4 * tau44(p, q)&
            )
    
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
    
            tau147(p, q) = tau147(p, q) + ( &
                4 * tau44(q, p)&
            )
    
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
    
            tau148(p, q) = tau148(p, q) + ( &
                4 * tau44(q, p)&
            )
    
        end do
    end do
    !$omp end do

    deallocate(tau44)

    allocate(tau45(NAO, NAO))
    !$omp single
    tau45 = 0.0
    !$omp end single

    !$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
            do r=1, NAO
                tau45(p, q) = tau45(p, q) + ( &
                    t2(q, r) * H102(p, q, r)&
                )
            end do
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
    
            tau47(p, q) = tau47(p, q) + ( &
                2 * tau45(p, q)&
            )
    
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
    
            tau146(p, q) = tau146(p, q) + ( &
                2 * tau45(p, q)&
            )
    
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
    
            tau147(p, q) = tau147(p, q) + ( &
                2 * tau45(q, p)&
            )
    
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
    
            tau148(p, q) = tau148(p, q) + ( &
                2 * tau45(q, p)&
            )
    
        end do
    end do
    !$omp end do

    deallocate(tau45)

    allocate(tau46(NAO, NAO))
    !$omp single
    tau46 = 0.0
    !$omp end single

    !$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
            do r=1, NAO
                tau46(p, q) = tau46(p, q) + ( &
                    t1(r) * H111(p, q, r)&
                )
            end do
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
    
            tau47(p, q) = tau47(p, q) - ( &
                tau46(p, q)&
            )
    
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
    
            tau52(p, q) = tau52(p, q) + ( &
                2 * t1(q) * tau47(p, q)&
            )
    
        end do
    end do
    !$omp end do

    deallocate(tau47)

    !$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
    
            tau146(p, q) = tau146(p, q) - ( &
                tau46(p, q)&
            )
    
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
    
            tau147(p, q) = tau147(p, q) - ( &
                tau46(q, p)&
            )
    
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
    
            tau148(p, q) = tau148(p, q) - ( &
                tau46(q, p)&
            )
    
        end do
    end do
    !$omp end do

    deallocate(tau46)

    allocate(tau49(NAO, NAO, NAO))
    !$omp single
    tau49 = 0.0
    !$omp end single

    !$omp do schedule(static)
    do r=1, NAO
        do q=1, NAO
            do p=1, NAO
                do s=1, NAO
                    tau49(p, q, r) = tau49(p, q, r) + ( &
                        t2(r, s) * H003(p, s, q)&
                    )
                end do
            end do
        end do
    end do
    !$omp end do

    allocate(tau50(NAO, NAO, NAO))
    !$omp single
    tau50 = 0.0
    !$omp end single

    !$omp do schedule(static)
    do r=1, NAO
        do q=1, NAO
            do p=1, NAO
    
                tau50(p, q, r) = tau50(p, q, r) + ( &
                    3 * tau49(q, p, r)&
                )
    
            end do
        end do
    end do
    !$omp end do

    allocate(tau82(NAO, NAO, NAO))
    !$omp single
    tau82 = 0.0
    !$omp end single

    !$omp do schedule(static)
    do r=1, NAO
        do q=1, NAO
            do p=1, NAO
                do s=1, NAO
                    tau82(p, q, r) = tau82(p, q, r) + ( &
                        t2(r, s) * tau49(s, p, q)&
                    )
                end do
            end do
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do r=1, NAO
        do q=1, NAO
            do p=1, NAO
    
                tau84(p, q, r) = tau84(p, q, r) + ( &
                    3 * tau82(p, r, q)&
                )
    
            end do
        end do
    end do
    !$omp end do

    allocate(tau88(NAO, NAO, NAO))
    !$omp single
    tau88 = 0.0
    !$omp end single

    !$omp do schedule(static)
    do r=1, NAO
        do q=1, NAO
            do p=1, NAO
    
                tau88(p, q, r) = tau88(p, q, r) - ( &
                    2 * t1(p)**2 * tau84(p, r, q)&
                )
    
            end do
        end do
    end do
    !$omp end do

    deallocate(tau84)

    allocate(tau152(NAO, NAO, NAO))
    !$omp single
    tau152 = 0.0
    !$omp end single

    !$omp do schedule(static)
    do r=1, NAO
        do q=1, NAO
            do p=1, NAO
    
                tau152(p, q, r) = tau152(p, q, r) + ( &
                    6 * tau82(p, r, q)&
                )
    
            end do
        end do
    end do
    !$omp end do

    deallocate(tau82)

    allocate(tau134(NAO, NAO, NAO))
    !$omp single
    tau134 = 0.0
    !$omp end single

    !$omp do schedule(static)
    do r=1, NAO
        do q=1, NAO
            do p=1, NAO
    
                tau134(p, q, r) = tau134(p, q, r) + ( &
                    3 * tau49(r, q, p)&
                )
    
            end do
        end do
    end do
    !$omp end do

    deallocate(tau49)

    !$omp do schedule(static)
    do r=1, NAO
        do q=1, NAO
            do p=1, NAO
    
                tau50(p, q, r) = tau50(p, q, r) + ( &
                    H102(r, q, p)&
                )
    
            end do
        end do
    end do
    !$omp end do

    allocate(tau51(NAO, NAO))
    !$omp single
    tau51 = 0.0
    !$omp end single

    !$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
            do s=1, NAO
                do r=1, NAO
                    tau51(p, q) = tau51(p, q) + ( &
                        t3(r, p, s) * tau50(r, s, q)&
                    )
                end do
            end do
        end do
    end do
    !$omp end do

    deallocate(tau50)

    !$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
    
            tau52(p, q) = tau52(p, q) - ( &
                tau51(q, p)&
            )
    
        end do
    end do
    !$omp end do

    deallocate(tau51)

    !$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
    
            tau52(p, q) = tau52(p, q) - ( &
                8 * t2(q, p)**2 * H012(p, p, q)&
            )
    
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
    
            tau52(p, q) = tau52(p, q) - ( &
                2 * t1(q)**3 * H102(p, q, q)&
            )
    
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
    
            Res2(p, q) = Res2(p, q) - ( &
                tau52(p, q)&
            )
    
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
    
            Res2(p, q) = Res2(p, q) - ( &
                tau52(q, p)&
            )
    
        end do
    end do
    !$omp end do

    deallocate(tau52)

    allocate(tau53(NAO, NAO))
    !$omp single
    tau53 = 0.0
    !$omp end single

    !$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
            do r=1, NAO
                tau53(p, q) = tau53(p, q) + ( &
                    H003(p, r, q) * t3(p, r, q)&
                )
            end do
        end do
    end do
    !$omp end do

    allocate(tau60(NAO, NAO))
    !$omp single
    tau60 = 0.0
    !$omp end single

    !$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
    
            tau60(p, q) = tau60(p, q) + ( &
                6 * tau53(q, p)&
            )
    
        end do
    end do
    !$omp end do

    allocate(tau64(NAO, NAO))
    !$omp single
    tau64 = 0.0
    !$omp end single

    !$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
    
            tau64(p, q) = tau64(p, q) + ( &
                3 * tau53(q, p)&
            )
    
        end do
    end do
    !$omp end do

    allocate(tau127(NAO, NAO))
    !$omp single
    tau127 = 0.0
    !$omp end single

    !$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
    
            tau127(p, q) = tau127(p, q) + ( &
                3 * tau53(q, p)&
            )
    
        end do
    end do
    !$omp end do

    allocate(tau128(NAO, NAO))
    !$omp single
    tau128 = 0.0
    !$omp end single

    !$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
    
            tau128(p, q) = tau128(p, q) + ( &
                3 * tau53(q, p)&
            )
    
        end do
    end do
    !$omp end do

    allocate(tau130(NAO, NAO))
    !$omp single
    tau130 = 0.0
    !$omp end single

    !$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
    
            tau130(p, q) = tau130(p, q) - ( &
                6 * tau53(q, p)&
            )
    
        end do
    end do
    !$omp end do

    deallocate(tau53)

    allocate(tau54(NAO, NAO))
    !$omp single
    tau54 = 0.0
    !$omp end single

    !$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
            do r=1, NAO
                tau54(p, q) = tau54(p, q) + ( &
                    t1(r) * H021(p, q, r)&
                )
            end do
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
    
            tau60(p, q) = tau60(p, q) + ( &
                2 * tau54(q, p)&
            )
    
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
    
            tau64(p, q) = tau64(p, q) + ( &
                tau54(q, p)&
            )
    
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
    
            tau127(p, q) = tau127(p, q) + ( &
                tau54(q, p)&
            )
    
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
    
            tau128(p, q) = tau128(p, q) + ( &
                tau54(q, p)&
            )
    
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
    
            tau130(p, q) = tau130(p, q) - ( &
                2 * tau54(q, p)&
            )
    
        end do
    end do
    !$omp end do

    deallocate(tau54)

    allocate(tau55(NAO, NAO))
    !$omp single
    tau55 = 0.0
    !$omp end single

    !$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
            do r=1, NAO
                tau55(p, q) = tau55(p, q) + ( &
                    t2(q, r) * H012(p, q, r)&
                )
            end do
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
    
            tau56(p, q) = tau56(p, q) - ( &
                tau55(p, q)&
            )
    
        end do
    end do
    !$omp end do

    allocate(tau66(NAO, NAO))
    !$omp single
    tau66 = 0.0
    !$omp end single

    !$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
    
            tau66(p, q) = tau66(p, q) + ( &
                tau55(p, q)&
            )
    
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
    
            tau66(p, q) = tau66(p, q) + ( &
                tau55(q, p)&
            )
    
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
    
            Res2(p, q) = Res2(p, q) - ( &
                8 * t1(p) * t1(q) * tau66(p, q)&
            )
    
        end do
    end do
    !$omp end do

    deallocate(tau66)

    allocate(tau126(NAO, NAO))
    !$omp single
    tau126 = 0.0
    !$omp end single

    !$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
    
            tau126(p, q) = tau126(p, q) - ( &
                tau55(p, q)&
            )
    
        end do
    end do
    !$omp end do

    deallocate(tau55)

    !$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
    
            tau56(p, q) = tau56(p, q) + ( &
                3 * H030(p, p, q)&
            )
    
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
    
            tau60(p, q) = tau60(p, q) + ( &
                2 * tau56(p, q)&
            )
    
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
    
            tau60(p, q) = tau60(p, q) + ( &
                2 * tau56(q, p)&
            )
    
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
    
            tau130(p, q) = tau130(p, q) - ( &
                2 * tau56(p, q)&
            )
    
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
    
            tau130(p, q) = tau130(p, q) - ( &
                2 * tau56(q, p)&
            )
    
        end do
    end do
    !$omp end do

    deallocate(tau56)

    !$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
    
            tau57(p, q) = tau57(p, q) + ( &
                H002(q, p)&
            )
    
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
    
            tau57(p, q) = tau57(p, q) + ( &
                2 * H012(p, p, q)&
            )
    
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
    
            tau57(p, q) = tau57(p, q) + ( &
                2 * H012(q, q, p)&
            )
    
        end do
    end do
    !$omp end do

    allocate(tau58(NAO, NAO))
    !$omp single
    tau58 = 0.0
    !$omp end single

    !$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
    
            tau58(p, q) = tau58(p, q) + ( &
                2 * t1(p) * tau57(q, p)&
            )
    
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
    
            tau61(p, q) = tau61(p, q) + ( &
                2 * t1(p) * tau57(q, p)&
            )
    
        end do
    end do
    !$omp end do

    allocate(tau71(NAO, NAO))
    !$omp single
    tau71 = 0.0
    !$omp end single

    !$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
    
            tau71(p, q) = tau71(p, q) + ( &
                2 * t1(p)**2 * tau57(q, p)&
            )
    
        end do
    end do
    !$omp end do

    allocate(tau122(NAO, NAO, NAO))
    !$omp single
    tau122 = 0.0
    !$omp end single

    !$omp do schedule(static)
    do r=1, NAO
        do q=1, NAO
            do p=1, NAO
    
                tau122(p, q, r) = tau122(p, q, r) + ( &
                    2 * tau57(q, p) * t3(r, p, q)&
                )
    
            end do
        end do
    end do
    !$omp end do

    allocate(tau123(NAO, NAO, NAO))
    !$omp single
    tau123 = 0.0
    !$omp end single

    !$omp do schedule(static)
    do r=1, NAO
        do q=1, NAO
            do p=1, NAO
    
                tau123(p, q, r) = tau123(p, q, r) - ( &
                    2 * tau57(q, p) * t3(r, p, q)&
                )
    
            end do
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
    
            tau127(p, q) = tau127(p, q) + ( &
                t2(q, p) * tau57(q, p)&
            )
    
        end do
    end do
    !$omp end do

    allocate(tau132(NAO, NAO, NAO))
    !$omp single
    tau132 = 0.0
    !$omp end single

    !$omp do schedule(static)
    do r=1, NAO
        do q=1, NAO
            do p=1, NAO
    
                tau132(p, q, r) = tau132(p, q, r) + ( &
                    2 * t2(q, p) * tau57(r, q)&
                )
    
            end do
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
    
            tau58(p, q) = tau58(p, q) - ( &
                H011(p, q)&
            )
    
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
    
            tau58(p, q) = tau58(p, q) - ( &
                2 * H021(p, p, q)&
            )
    
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
    
            tau58(p, q) = tau58(p, q) - ( &
                4 * H021(q, p, q)&
            )
    
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
    
            tau60(p, q) = tau60(p, q) + ( &
                t1(q) * tau58(p, q)&
            )
    
        end do
    end do
    !$omp end do

    deallocate(tau58)

    allocate(tau59(NAO, NAO))
    !$omp single
    tau59 = 0.0
    !$omp end single

    !$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
    
            tau59(p, q) = tau59(p, q) + ( &
                H011(p, q)&
            )
    
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
    
            tau59(p, q) = tau59(p, q) + ( &
                2 * H021(p, p, q)&
            )
    
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
    
            tau59(p, q) = tau59(p, q) + ( &
                4 * H021(q, p, q)&
            )
    
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
    
            tau60(p, q) = tau60(p, q) - ( &
                t1(p) * tau59(q, p)&
            )
    
        end do
    end do
    !$omp end do

    allocate(tau65(NAO, NAO))
    !$omp single
    tau65 = 0.0
    !$omp end single

    !$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
    
            tau65(p, q) = tau65(p, q) + ( &
                t1(p)**2 * tau59(q, p)&
            )
    
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
    
            tau130(p, q) = tau130(p, q) + ( &
                t1(q) * tau59(p, q)&
            )
    
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
    
            tau130(p, q) = tau130(p, q) + ( &
                t1(p) * tau59(q, p)&
            )
    
        end do
    end do
    !$omp end do

    deallocate(tau59)

    !$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
    
            tau60(p, q) = tau60(p, q) + ( &
                2 * H020(q, p)&
            )
    
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
    
            Res2(p, q) = Res2(p, q) + ( &
                4 * t2(q, p) * tau60(q, p)&
            )
    
        end do
    end do
    !$omp end do

    deallocate(tau60)

    !$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
    
            tau61(p, q) = tau61(p, q) - ( &
                H011(p, q)&
            )
    
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
    
            tau61(p, q) = tau61(p, q) - ( &
                2 * H021(p, p, q)&
            )
    
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
    
            tau61(p, q) = tau61(p, q) - ( &
                4 * H021(q, p, q)&
            )
    
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
    
            Res2(p, q) = Res2(p, q) + ( &
                2 * tau61(p, q) * t3(q, q, p)&
            )
    
        end do
    end do
    !$omp end do

    !$omp single
    Res3 = 0.0
    !$omp end single

    !$omp do schedule(static)
    do r=1, NAO
        do q=1, NAO
            do p=1, NAO
    
                Res3(p, q, r) = Res3(p, q, r) + ( &
                    2 * t1(p)**2 * t2(r, q) * tau61(q, p)&
                )
    
            end do
        end do
    end do
    !$omp end do

    deallocate(tau61)

    !$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
    
            tau62(p, q) = tau62(p, q) + ( &
                H011(p, q)&
            )
    
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
    
            tau62(p, q) = tau62(p, q) + ( &
                2 * H021(p, p, q)&
            )
    
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
    
            tau62(p, q) = tau62(p, q) + ( &
                4 * H021(q, p, q)&
            )
    
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do r=1, NAO
        do q=1, NAO
            do p=1, NAO
    
                tau122(p, q, r) = tau122(p, q, r) - ( &
                    t2(r, q) * tau62(p, q)&
                )
    
            end do
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do r=1, NAO
        do q=1, NAO
            do p=1, NAO
    
                tau122(p, q, r) = tau122(p, q, r) - ( &
                    t2(r, p) * tau62(q, p)&
                )
    
            end do
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do r=1, NAO
        do q=1, NAO
            do p=1, NAO
    
                tau123(p, q, r) = tau123(p, q, r) + ( &
                    t2(r, q) * tau62(p, q)&
                )
    
            end do
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do r=1, NAO
        do q=1, NAO
            do p=1, NAO
    
                tau123(p, q, r) = tau123(p, q, r) + ( &
                    t2(r, p) * tau62(q, p)&
                )
    
            end do
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
    
            Res2(p, q) = Res2(p, q) - ( &
                2 * t1(p)**2 * t1(q) * tau62(q, p)&
            )
    
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do r=1, NAO
        do q=1, NAO
            do p=1, NAO
    
                Res3(p, q, r) = Res3(p, q, r) - ( &
                    2 * t1(q)**2 * t2(r, p) * tau62(p, q)&
                )
    
            end do
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
    
        tau63(p) = tau63(p) + ( &
            H001(p)&
        )
    
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
    
        tau63(p) = tau63(p) + ( &
            2 * H011(p, p)&
        )
    
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
    
        tau63(p) = tau63(p) + ( &
            4 * H021(p, p, p)&
        )
    
    end do
    !$omp end do

    !$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
    
            tau65(p, q) = tau65(p, q) + ( &
                tau63(q) * t2(q, p)&
            )
    
        end do
    end do
    !$omp end do

    deallocate(tau63)

    !$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
    
            tau64(p, q) = tau64(p, q) + ( &
                H020(q, p)&
            )
    
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
    
            tau64(p, q) = tau64(p, q) + ( &
                3 * H030(p, p, q)&
            )
    
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
    
            tau64(p, q) = tau64(p, q) + ( &
                3 * H030(q, q, p)&
            )
    
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
    
            tau65(p, q) = tau65(p, q) - ( &
                4 * t1(p) * tau64(q, p)&
            )
    
        end do
    end do
    !$omp end do

    deallocate(tau64)

    !$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
    
            tau65(p, q) = tau65(p, q) - ( &
                H110(p, q)&
            )
    
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
    
            tau65(p, q) = tau65(p, q) - ( &
                2 * H120(p, q, q)&
            )
    
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
    
            Res2(p, q) = Res2(p, q) - ( &
                2 * t1(p) * tau65(q, p)&
            )
    
        end do
    end do
    !$omp end do

    deallocate(tau65)

    allocate(tau68(NAO, NAO))
    !$omp single
    tau68 = 0.0
    !$omp end single

    !$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
    
            tau68(p, q) = tau68(p, q) + ( &
                t3(p, p, q)&
            )
    
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
    
            tau68(p, q) = tau68(p, q) + ( &
                2 * t1(p) * t2(q, p)&
            )
    
        end do
    end do
    !$omp end do

    allocate(tau108(NAO, NAO, NAO))
    !$omp single
    tau108 = 0.0
    !$omp end single

    !$omp do schedule(static)
    do r=1, NAO
        do q=1, NAO
            do p=1, NAO
    
                tau108(p, q, r) = tau108(p, q, r) + ( &
                    2 * t1(p)**2 * t1(r) * tau68(q, r)&
                )
    
            end do
        end do
    end do
    !$omp end do

    allocate(tau125(NAO, NAO, NAO))
    !$omp single
    tau125 = 0.0
    !$omp end single

    !$omp do schedule(static)
    do r=1, NAO
        do q=1, NAO
            do p=1, NAO
    
                tau125(p, q, r) = tau125(p, q, r) + ( &
                    t1(p)**2 * tau68(r, q)&
                )
    
            end do
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
    
            Res2(p, q) = Res2(p, q) - ( &
                tau18(q) * tau68(q, p)&
            )
    
        end do
    end do
    !$omp end do

    deallocate(tau18)

    !$omp do schedule(static)
    do p=1, NAO
    
        tau70(p) = tau70(p) + ( &
            H010(p)&
        )
    
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
    
        tau70(p) = tau70(p) + ( &
            2 * H020(p, p)&
        )
    
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
    
        tau70(p) = tau70(p) + ( &
            4 * H030(p, p, p)&
        )
    
    end do
    !$omp end do

    !$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
    
            Res2(p, q) = Res2(p, q) + ( &
                2 * tau70(p) * t2(q, p)&
            )
    
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
    
            Res2(p, q) = Res2(p, q) + ( &
                2 * tau70(q) * t2(q, p)&
            )
    
        end do
    end do
    !$omp end do

    deallocate(tau70)

    !$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
    
            tau71(p, q) = tau71(p, q) - ( &
                H101(p, q)&
            )
    
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
    
            tau71(p, q) = tau71(p, q) - ( &
                2 * H111(p, q, q)&
            )
    
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
    
            Res2(p, q) = Res2(p, q) + ( &
                t1(p)**2 * tau71(q, p)&
            )
    
        end do
    end do
    !$omp end do

    deallocate(tau71)

    !$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
    
            tau73(p, q) = tau73(p, q) + ( &
                H101(q, p)&
            )
    
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
            do r=1, NAO
                Res2(p, q) = Res2(p, q) + ( &
                    t2(r, q) * tau73(r, p)&
                )
            end do
        end do
    end do
    !$omp end do

    deallocate(tau73)

    allocate(tau74(NAO, NAO))
    !$omp single
    tau74 = 0.0
    !$omp end single

    !$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
    
            tau74(p, q) = tau74(p, q) + ( &
                H110(p, q)&
            )
    
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
    
            tau74(p, q) = tau74(p, q) + ( &
                2 * H120(p, q, q)&
            )
    
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
    
            Res2(p, q) = Res2(p, q) + ( &
                2 * t1(q) * tau74(p, q)&
            )
    
        end do
    end do
    !$omp end do

    deallocate(tau74)

    allocate(tau75(NAO, NAO))
    !$omp single
    tau75 = 0.0
    !$omp end single

    !$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
    
            tau75(p, q) = tau75(p, q) + ( &
                H101(p, q)&
            )
    
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
    
            tau75(p, q) = tau75(p, q) + ( &
                2 * H111(p, q, q)&
            )
    
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
    
            Res2(p, q) = Res2(p, q) - ( &
                t1(q)**2 * tau75(p, q)&
            )
    
        end do
    end do
    !$omp end do

    deallocate(tau75)

    !$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
    
            tau76(p, q) = tau76(p, q) + ( &
                H101(q, p)&
            )
    
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
    
            tau76(p, q) = tau76(p, q) - ( &
                2 * t1(p) * H102(q, p, p)&
            )
    
        end do
    end do
    !$omp end do

    allocate(tau77(NAO, NAO, NAO))
    !$omp single
    tau77 = 0.0
    !$omp end single

    !$omp do schedule(static)
    do r=1, NAO
        do q=1, NAO
            do p=1, NAO
                do s=1, NAO
                    tau77(p, q, r) = tau77(p, q, r) + ( &
                        tau76(s, r) * t3(s, p, q)&
                    )
                end do
            end do
        end do
    end do
    !$omp end do

    deallocate(tau76)

    !$omp do schedule(static)
    do r=1, NAO
        do q=1, NAO
            do p=1, NAO
    
                tau88(p, q, r) = tau88(p, q, r) + ( &
                    tau77(q, r, p)&
                )
    
            end do
        end do
    end do
    !$omp end do

    deallocate(tau77)

    allocate(tau78(NAO, NAO, NAO))
    !$omp single
    tau78 = 0.0
    !$omp end single

    !$omp do schedule(static)
    do r=1, NAO
        do q=1, NAO
            do p=1, NAO
                do s=1, NAO
                    tau78(p, q, r) = tau78(p, q, r) + ( &
                        t2(r, s) * H012(p, s, q)&
                    )
                end do
            end do
        end do
    end do
    !$omp end do

    allocate(tau79(NAO, NAO, NAO))
    !$omp single
    tau79 = 0.0
    !$omp end single

    !$omp do schedule(static)
    do r=1, NAO
        do q=1, NAO
            do p=1, NAO
                do s=1, NAO
                    tau79(p, q, r) = tau79(p, q, r) + ( &
                        t2(r, s) * tau78(p, s, q)&
                    )
                end do
            end do
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do r=1, NAO
        do q=1, NAO
            do p=1, NAO
    
                tau81(p, q, r) = tau81(p, q, r) + ( &
                    2 * tau79(p, r, q)&
                )
    
            end do
        end do
    end do
    !$omp end do

    deallocate(tau79)

    !$omp do schedule(static)
    do r=1, NAO
        do q=1, NAO
            do p=1, NAO
    
                tau88(p, q, r) = tau88(p, q, r) + ( &
                    2 * t1(p) * tau81(p, r, q)&
                )
    
            end do
        end do
    end do
    !$omp end do

    deallocate(tau81)

    !$omp do schedule(static)
    do r=1, NAO
        do q=1, NAO
            do p=1, NAO
    
                tau132(p, q, r) = tau132(p, q, r) - ( &
                    2 * tau78(q, r, p)&
                )
    
            end do
        end do
    end do
    !$omp end do

    allocate(tau135(NAO, NAO, NAO))
    !$omp single
    tau135 = 0.0
    !$omp end single

    !$omp do schedule(static)
    do r=1, NAO
        do q=1, NAO
            do p=1, NAO
    
                tau135(p, q, r) = tau135(p, q, r) - ( &
                    2 * tau78(q, r, p)&
                )
    
            end do
        end do
    end do
    !$omp end do

    allocate(tau136(NAO, NAO, NAO))
    !$omp single
    tau136 = 0.0
    !$omp end single

    !$omp do schedule(static)
    do r=1, NAO
        do q=1, NAO
            do p=1, NAO
    
                tau136(p, q, r) = tau136(p, q, r) - ( &
                    t1(q) * tau78(p, q, r)&
                )
    
            end do
        end do
    end do
    !$omp end do

    allocate(tau138(NAO, NAO, NAO))
    !$omp single
    tau138 = 0.0
    !$omp end single

    !$omp do schedule(static)
    do r=1, NAO
        do q=1, NAO
            do p=1, NAO
    
                tau138(p, q, r) = tau138(p, q, r) + ( &
                    2 * tau78(q, r, p)&
                )
    
            end do
        end do
    end do
    !$omp end do

    allocate(tau140(NAO, NAO, NAO))
    !$omp single
    tau140 = 0.0
    !$omp end single

    !$omp do schedule(static)
    do r=1, NAO
        do q=1, NAO
            do p=1, NAO
    
                tau140(p, q, r) = tau140(p, q, r) + ( &
                    2 * tau78(p, q, r)&
                )
    
            end do
        end do
    end do
    !$omp end do

    allocate(tau142(NAO, NAO, NAO))
    !$omp single
    tau142 = 0.0
    !$omp end single

    !$omp do schedule(static)
    do r=1, NAO
        do q=1, NAO
            do p=1, NAO
    
                tau142(p, q, r) = tau142(p, q, r) + ( &
                    2 * tau78(p, q, r)&
                )
    
            end do
        end do
    end do
    !$omp end do

    deallocate(tau78)

    allocate(tau85(NAO, NAO, NAO))
    !$omp single
    tau85 = 0.0
    !$omp end single

    !$omp do schedule(static)
    do r=1, NAO
        do q=1, NAO
            do p=1, NAO
                do s=1, NAO
                    tau85(p, q, r) = tau85(p, q, r) + ( &
                        t2(r, s) * H102(p, s, q)&
                    )
                end do
            end do
        end do
    end do
    !$omp end do

    allocate(tau86(NAO, NAO, NAO))
    !$omp single
    tau86 = 0.0
    !$omp end single

    !$omp do schedule(static)
    do r=1, NAO
        do q=1, NAO
            do p=1, NAO
    
                tau86(p, q, r) = tau86(p, q, r) + ( &
                    tau85(p, q, r)&
                )
    
            end do
        end do
    end do
    !$omp end do

    allocate(tau96(NAO, NAO, NAO))
    !$omp single
    tau96 = 0.0
    !$omp end single

    !$omp do schedule(static)
    do r=1, NAO
        do q=1, NAO
            do p=1, NAO
    
                tau96(p, q, r) = tau96(p, q, r) + ( &
                    tau85(p, q, r)&
                )
    
            end do
        end do
    end do
    !$omp end do

    deallocate(tau85)

    !$omp do schedule(static)
    do r=1, NAO
        do q=1, NAO
            do p=1, NAO
    
                tau86(p, q, r) = tau86(p, q, r) - ( &
                    t2(r, q) * H102(p, q, q)&
                )
    
            end do
        end do
    end do
    !$omp end do

    allocate(tau87(NAO, NAO, NAO))
    !$omp single
    tau87 = 0.0
    !$omp end single

    !$omp do schedule(static)
    do r=1, NAO
        do q=1, NAO
            do p=1, NAO
                do s=1, NAO
                    tau87(p, q, r) = tau87(p, q, r) + ( &
                        t2(s, p) * tau86(q, s, r)&
                    )
                end do
            end do
        end do
    end do
    !$omp end do

    deallocate(tau86)

    !$omp do schedule(static)
    do r=1, NAO
        do q=1, NAO
            do p=1, NAO
    
                tau88(p, q, r) = tau88(p, q, r) + ( &
                    2 * tau87(q, p, r)&
                )
    
            end do
        end do
    end do
    !$omp end do

    deallocate(tau87)

    !$omp do schedule(static)
    do r=1, NAO
        do q=1, NAO
            do p=1, NAO
    
                Res3(p, q, r) = Res3(p, q, r) + ( &
                    tau88(p, q, r)&
                )
    
            end do
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do r=1, NAO
        do q=1, NAO
            do p=1, NAO
    
                Res3(p, q, r) = Res3(p, q, r) + ( &
                    tau88(q, p, r)&
                )
    
            end do
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do r=1, NAO
        do q=1, NAO
            do p=1, NAO
    
                Res3(p, q, r) = Res3(p, q, r) + ( &
                    tau88(r, p, q)&
                )
    
            end do
        end do
    end do
    !$omp end do

    deallocate(tau88)

    !$omp do schedule(static)
    do r=1, NAO
        do q=1, NAO
            do p=1, NAO
    
                tau90(p, q, r) = tau90(p, q, r) + ( &
                    4 * t1(r) * H210(q, p, r)&
                )
    
            end do
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do r=1, NAO
        do q=1, NAO
            do p=1, NAO
    
                Res3(p, q, r) = Res3(p, q, r) + ( &
                    tau90(p, q, r)&
                )
    
            end do
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do r=1, NAO
        do q=1, NAO
            do p=1, NAO
    
                Res3(p, q, r) = Res3(p, q, r) + ( &
                    tau90(p, r, q)&
                )
    
            end do
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do r=1, NAO
        do q=1, NAO
            do p=1, NAO
    
                Res3(p, q, r) = Res3(p, q, r) + ( &
                    tau90(q, r, p)&
                )
    
            end do
        end do
    end do
    !$omp end do

    deallocate(tau90)

    allocate(tau91(NAO, NAO, NAO))
    !$omp single
    tau91 = 0.0
    !$omp end single

    !$omp do schedule(static)
    do r=1, NAO
        do q=1, NAO
            do p=1, NAO
                do s=1, NAO
                    tau91(p, q, r) = tau91(p, q, r) + ( &
                        H021(p, q, s) * t3(r, s, p)&
                    )
                end do
            end do
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do r=1, NAO
        do q=1, NAO
            do p=1, NAO
    
                tau94(p, q, r) = tau94(p, q, r) - ( &
                    4 * tau91(p, q, r)&
                )
    
            end do
        end do
    end do
    !$omp end do

    deallocate(tau91)

    allocate(tau92(NAO, NAO, NAO))
    !$omp single
    tau92 = 0.0
    !$omp end single

    !$omp do schedule(static)
    do r=1, NAO
        do q=1, NAO
            do p=1, NAO
                do s=1, NAO
                    tau92(p, q, r) = tau92(p, q, r) + ( &
                        H003(p, q, s) * t3(r, s, p)&
                    )
                end do
            end do
        end do
    end do
    !$omp end do

    allocate(tau93(NAO, NAO, NAO))
    !$omp single
    tau93 = 0.0
    !$omp end single

    !$omp do schedule(static)
    do r=1, NAO
        do q=1, NAO
            do p=1, NAO
                do s=1, NAO
                    tau93(p, q, r) = tau93(p, q, r) + ( &
                        t2(q, s) * tau92(p, s, r)&
                    )
                end do
            end do
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do r=1, NAO
        do q=1, NAO
            do p=1, NAO
    
                tau94(p, q, r) = tau94(p, q, r) + ( &
                    6 * tau93(q, p, r)&
                )
    
            end do
        end do
    end do
    !$omp end do

    deallocate(tau93)

    !$omp do schedule(static)
    do r=1, NAO
        do q=1, NAO
            do p=1, NAO
    
                tau100(p, q, r) = tau100(p, q, r) + ( &
                    t1(p) * tau94(q, p, r)&
                )
    
            end do
        end do
    end do
    !$omp end do

    deallocate(tau94)

    !$omp do schedule(static)
    do r=1, NAO
        do q=1, NAO
            do p=1, NAO
    
                tau132(p, q, r) = tau132(p, q, r) + ( &
                    6 * tau92(q, r, p)&
                )
    
            end do
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do r=1, NAO
        do q=1, NAO
            do p=1, NAO
    
                tau135(p, q, r) = tau135(p, q, r) + ( &
                    6 * tau92(q, r, p)&
                )
    
            end do
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do r=1, NAO
        do q=1, NAO
            do p=1, NAO
    
                tau136(p, q, r) = tau136(p, q, r) + ( &
                    3 * t1(p) * tau92(q, p, r)&
                )
    
            end do
        end do
    end do
    !$omp end do

    allocate(tau137(NAO, NAO, NAO))
    !$omp single
    tau137 = 0.0
    !$omp end single

    !$omp do schedule(static)
    do r=1, NAO
        do q=1, NAO
            do p=1, NAO
    
                tau137(p, q, r) = tau137(p, q, r) + ( &
                    2 * tau136(q, r, p)&
                )
    
            end do
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do r=1, NAO
        do q=1, NAO
            do p=1, NAO
    
                tau137(p, q, r) = tau137(p, q, r) + ( &
                    2 * tau136(r, q, p)&
                )
    
            end do
        end do
    end do
    !$omp end do

    deallocate(tau136)

    !$omp do schedule(static)
    do r=1, NAO
        do q=1, NAO
            do p=1, NAO
    
                tau138(p, q, r) = tau138(p, q, r) - ( &
                    6 * tau92(q, r, p)&
                )
    
            end do
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do r=1, NAO
        do q=1, NAO
            do p=1, NAO
    
                tau140(p, q, r) = tau140(p, q, r) - ( &
                    6 * tau92(p, q, r)&
                )
    
            end do
        end do
    end do
    !$omp end do

    deallocate(tau92)

    allocate(tau95(NAO, NAO, NAO))
    !$omp single
    tau95 = 0.0
    !$omp end single

    !$omp do schedule(static)
    do r=1, NAO
        do q=1, NAO
            do p=1, NAO
                do s=1, NAO
                    tau95(p, q, r) = tau95(p, q, r) + ( &
                        H012(p, s, q) * t3(r, s, p)&
                    )
                end do
            end do
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do r=1, NAO
        do q=1, NAO
            do p=1, NAO
    
                tau96(p, q, r) = tau96(p, q, r) + ( &
                    2 * tau95(p, q, r)&
                )
    
            end do
        end do
    end do
    !$omp end do

    deallocate(tau95)

    !$omp do schedule(static)
    do r=1, NAO
        do q=1, NAO
            do p=1, NAO
    
                tau100(p, q, r) = tau100(p, q, r) + ( &
                    t1(q)**2 * tau96(p, q, r)&
                )
    
            end do
        end do
    end do
    !$omp end do

    deallocate(tau96)

    allocate(tau97(NAO, NAO, NAO))
    !$omp single
    tau97 = 0.0
    !$omp end single

    !$omp do schedule(static)
    do r=1, NAO
        do q=1, NAO
            do p=1, NAO
                do s=1, NAO
                    tau97(p, q, r) = tau97(p, q, r) + ( &
                        H102(p, q, s) * t3(r, s, q)&
                    )
                end do
            end do
        end do
    end do
    !$omp end do

    allocate(tau99(NAO, NAO, NAO))
    !$omp single
    tau99 = 0.0
    !$omp end single

    !$omp do schedule(static)
    do r=1, NAO
        do q=1, NAO
            do p=1, NAO
    
                tau99(p, q, r) = tau99(p, q, r) + ( &
                    2 * tau97(p, q, r)&
                )
    
            end do
        end do
    end do
    !$omp end do

    deallocate(tau97)

    allocate(tau98(NAO, NAO, NAO))
    !$omp single
    tau98 = 0.0
    !$omp end single

    !$omp do schedule(static)
    do r=1, NAO
        do q=1, NAO
            do p=1, NAO
                do s=1, NAO
                    tau98(p, q, r) = tau98(p, q, r) + ( &
                        t2(r, s) * H111(p, q, s)&
                    )
                end do
            end do
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do r=1, NAO
        do q=1, NAO
            do p=1, NAO
    
                tau99(p, q, r) = tau99(p, q, r) - ( &
                    tau98(p, q, r)&
                )
    
            end do
        end do
    end do
    !$omp end do

    deallocate(tau98)

    !$omp do schedule(static)
    do r=1, NAO
        do q=1, NAO
            do p=1, NAO
    
                tau100(p, q, r) = tau100(p, q, r) + ( &
                    t1(q) * tau99(p, q, r)&
                )
    
            end do
        end do
    end do
    !$omp end do

    deallocate(tau99)

    !$omp do schedule(static)
    do r=1, NAO
        do q=1, NAO
            do p=1, NAO
    
                Res3(p, q, r) = Res3(p, q, r) - ( &
                    2 * tau100(p, q, r)&
                )
    
            end do
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do r=1, NAO
        do q=1, NAO
            do p=1, NAO
    
                Res3(p, q, r) = Res3(p, q, r) - ( &
                    2 * tau100(p, r, q)&
                )
    
            end do
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do r=1, NAO
        do q=1, NAO
            do p=1, NAO
    
                Res3(p, q, r) = Res3(p, q, r) - ( &
                    2 * tau100(q, p, r)&
                )
    
            end do
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do r=1, NAO
        do q=1, NAO
            do p=1, NAO
    
                Res3(p, q, r) = Res3(p, q, r) - ( &
                    2 * tau100(q, r, p)&
                )
    
            end do
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do r=1, NAO
        do q=1, NAO
            do p=1, NAO
    
                Res3(p, q, r) = Res3(p, q, r) - ( &
                    2 * tau100(r, p, q)&
                )
    
            end do
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do r=1, NAO
        do q=1, NAO
            do p=1, NAO
    
                Res3(p, q, r) = Res3(p, q, r) - ( &
                    2 * tau100(r, q, p)&
                )
    
            end do
        end do
    end do
    !$omp end do

    deallocate(tau100)

    allocate(tau101(NAO, NAO, NAO))
    !$omp single
    tau101 = 0.0
    !$omp end single

    !$omp do schedule(static)
    do r=1, NAO
        do q=1, NAO
            do p=1, NAO
                do s=1, NAO
                    tau101(p, q, r) = tau101(p, q, r) + ( &
                        t2(r, s) * H201(p, q, s)&
                    )
                end do
            end do
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do r=1, NAO
        do q=1, NAO
            do p=1, NAO
    
                Res3(p, q, r) = Res3(p, q, r) + ( &
                    tau101(p, q, r)&
                )
    
            end do
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do r=1, NAO
        do q=1, NAO
            do p=1, NAO
    
                Res3(p, q, r) = Res3(p, q, r) + ( &
                    tau101(p, r, q)&
                )
    
            end do
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do r=1, NAO
        do q=1, NAO
            do p=1, NAO
    
                Res3(p, q, r) = Res3(p, q, r) + ( &
                    tau101(q, p, r)&
                )
    
            end do
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do r=1, NAO
        do q=1, NAO
            do p=1, NAO
    
                Res3(p, q, r) = Res3(p, q, r) + ( &
                    tau101(r, p, q)&
                )
    
            end do
        end do
    end do
    !$omp end do

    deallocate(tau101)

    !$omp do schedule(static)
    do r=1, NAO
        do q=1, NAO
            do p=1, NAO
    
                tau102(p, q, r) = tau102(p, q, r) + ( &
                    t1(q) * t2(r, p)&
                )
    
            end do
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do r=1, NAO
        do q=1, NAO
            do p=1, NAO
    
                tau102(p, q, r) = tau102(p, q, r) + ( &
                    t1(r) * t2(q, p)&
                )
    
            end do
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do r=1, NAO
        do q=1, NAO
            do p=1, NAO
    
                tau108(p, q, r) = tau108(p, q, r) + ( &
                    8 * t3(r, p, q) * tau102(r, p, q)&
                )
    
            end do
        end do
    end do
    !$omp end do

    deallocate(tau102)

    allocate(tau103(NAO, NAO, NAO))
    !$omp single
    tau103 = 0.0
    !$omp end single

    !$omp do schedule(static)
    do r=1, NAO
        do q=1, NAO
            do p=1, NAO
    
                tau103(p, q, r) = tau103(p, q, r) + ( &
                    t1(p) * t2(r, q)&
                )
    
            end do
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do r=1, NAO
        do q=1, NAO
            do p=1, NAO
    
                tau103(p, q, r) = tau103(p, q, r) + ( &
                    t1(r) * t2(q, p)&
                )
    
            end do
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do r=1, NAO
        do q=1, NAO
            do p=1, NAO
    
                tau108(p, q, r) = tau108(p, q, r) + ( &
                    8 * t1(q) * t2(r, p) * tau103(r, q, p)&
                )
    
            end do
        end do
    end do
    !$omp end do

    allocate(tau124(NAO, NAO, NAO))
    !$omp single
    tau124 = 0.0
    !$omp end single

    !$omp do schedule(static)
    do r=1, NAO
        do q=1, NAO
            do p=1, NAO
    
                tau124(p, q, r) = tau124(p, q, r) + ( &
                    4 * t2(r, p) * tau103(r, q, p)&
                )
    
            end do
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do r=1, NAO
        do q=1, NAO
            do p=1, NAO
    
                tau125(p, q, r) = tau125(p, q, r) + ( &
                    4 * t2(r, p) * tau103(r, q, p)&
                )
    
            end do
        end do
    end do
    !$omp end do

    allocate(tau104(NAO, NAO))
    !$omp single
    tau104 = 0.0
    !$omp end single

    !$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
    
            tau104(p, q) = tau104(p, q) + ( &
                t3(p, p, q)&
            )
    
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
    
            tau104(p, q) = tau104(p, q) + ( &
                t1(p)**2 * t1(q)&
            )
    
        end do
    end do
    !$omp end do

    allocate(tau105(NAO, NAO, NAO))
    !$omp single
    tau105 = 0.0
    !$omp end single

    !$omp do schedule(static)
    do r=1, NAO
        do q=1, NAO
            do p=1, NAO
    
                tau105(p, q, r) = tau105(p, q, r) + ( &
                    t1(q) * tau104(p, r)&
                )
    
            end do
        end do
    end do
    !$omp end do

    allocate(tau106(NAO, NAO, NAO))
    !$omp single
    tau106 = 0.0
    !$omp end single

    !$omp do schedule(static)
    do r=1, NAO
        do q=1, NAO
            do p=1, NAO
    
                tau106(p, q, r) = tau106(p, q, r) + ( &
                    t1(p) * tau104(q, r)&
                )
    
            end do
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do r=1, NAO
        do q=1, NAO
            do p=1, NAO
    
                tau108(p, q, r) = tau108(p, q, r) + ( &
                    2 * tau104(q, p) * t3(r, r, p)&
                )
    
            end do
        end do
    end do
    !$omp end do

    allocate(tau118(NAO, NAO, NAO))
    !$omp single
    tau118 = 0.0
    !$omp end single

    !$omp do schedule(static)
    do r=1, NAO
        do q=1, NAO
            do p=1, NAO
    
                tau118(p, q, r) = tau118(p, q, r) + ( &
                    t1(q) * tau104(p, r)&
                )
    
            end do
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do r=1, NAO
        do q=1, NAO
            do p=1, NAO
    
                tau124(p, q, r) = tau124(p, q, r) + ( &
                    2 * t2(q, p) * tau104(r, p)&
                )
    
            end do
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do r=1, NAO
        do q=1, NAO
            do p=1, NAO
    
                tau125(p, q, r) = tau125(p, q, r) + ( &
                    2 * t2(q, p) * tau104(r, p)&
                )
    
            end do
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do r=1, NAO
        do q=1, NAO
            do p=1, NAO
    
                Res3(p, q, r) = Res3(p, q, r) + ( &
                    2 * tau57(r, p) * tau125(p, q, r)&
                )
    
            end do
        end do
    end do
    !$omp end do

    deallocate(tau125)

    !$omp do schedule(static)
    do r=1, NAO
        do q=1, NAO
            do p=1, NAO
    
                tau105(p, q, r) = tau105(p, q, r) + ( &
                    t1(r) * t3(p, p, q)&
                )
    
            end do
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do r=1, NAO
        do q=1, NAO
            do p=1, NAO
    
                tau105(p, q, r) = tau105(p, q, r) + ( &
                    2 * t2(p, q) * t2(p, r)&
                )
    
            end do
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do r=1, NAO
        do q=1, NAO
            do p=1, NAO
    
                tau108(p, q, r) = tau108(p, q, r) + ( &
                    4 * t2(r, p) * tau105(q, r, p)&
                )
    
            end do
        end do
    end do
    !$omp end do

    deallocate(tau105)

    !$omp do schedule(static)
    do r=1, NAO
        do q=1, NAO
            do p=1, NAO
    
                tau106(p, q, r) = tau106(p, q, r) + ( &
                    2 * t1(q) * t1(r) * t2(p, q)&
                )
    
            end do
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do r=1, NAO
        do q=1, NAO
            do p=1, NAO
    
                tau108(p, q, r) = tau108(p, q, r) + ( &
                    4 * t2(q, p) * tau106(q, r, p)&
                )
    
            end do
        end do
    end do
    !$omp end do

    deallocate(tau106)

    allocate(tau107(NAO, NAO))
    !$omp single
    tau107 = 0.0
    !$omp end single

    !$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
    
            tau107(p, q) = tau107(p, q) + ( &
                2 * t1(p) * t3(q, q, p)&
            )
    
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
    
            tau107(p, q) = tau107(p, q) + ( &
                2 * t2(q, p)**2&
            )
    
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
    
            tau107(p, q) = tau107(p, q) + ( &
                t1(p)**2 * t1(q)**2&
            )
    
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do r=1, NAO
        do q=1, NAO
            do p=1, NAO
    
                tau108(p, q, r) = tau108(p, q, r) + ( &
                    t1(r)**2 * tau107(p, q)&
                )
    
            end do
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do r=1, NAO
        do q=1, NAO
            do p=1, NAO
    
                tau108(p, q, r) = tau108(p, q, r) + ( &
                    4 * t3(r, p, q)**2&
                )
    
            end do
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do r=1, NAO
        do q=1, NAO
            do p=1, NAO
    
                tau108(p, q, r) = tau108(p, q, r) + ( &
                    2 * t1(p)**2 * t2(r, q)**2&
                )
    
            end do
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do r=1, NAO
        do q=1, NAO
            do p=1, NAO
    
                tau108(p, q, r) = tau108(p, q, r) + ( &
                    2 * t1(q)**2 * t2(r, p)**2&
                )
    
            end do
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do r=1, NAO
        do q=1, NAO
            do p=1, NAO
    
                Res3(p, q, r) = Res3(p, q, r) - ( &
                    6 * H003(r, p, q) * tau108(p, r, q)&
                )
    
            end do
        end do
    end do
    !$omp end do

    deallocate(tau108)

    !$omp do schedule(static)
    do r=1, NAO
        do q=1, NAO
            do p=1, NAO
    
                tau109(p, q, r) = tau109(p, q, r) + ( &
                    t3(r, p, q)&
                )
    
            end do
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do r=1, NAO
        do q=1, NAO
            do p=1, NAO
    
                tau109(p, q, r) = tau109(p, q, r) + ( &
                    t1(r) * t2(q, p)&
                )
    
            end do
        end do
    end do
    !$omp end do

    allocate(tau112(NAO, NAO, NAO))
    !$omp single
    tau112 = 0.0
    !$omp end single

    !$omp do schedule(static)
    do r=1, NAO
        do q=1, NAO
            do p=1, NAO
    
                tau112(p, q, r) = tau112(p, q, r) + ( &
                    4 * t2(q, p) * tau109(r, p, q)&
                )
    
            end do
        end do
    end do
    !$omp end do

    allocate(tau114(NAO, NAO, NAO))
    !$omp single
    tau114 = 0.0
    !$omp end single

    !$omp do schedule(static)
    do r=1, NAO
        do q=1, NAO
            do p=1, NAO
    
                tau114(p, q, r) = tau114(p, q, r) + ( &
                    4 * t2(q, p) * tau109(r, p, q)&
                )
    
            end do
        end do
    end do
    !$omp end do

    allocate(tau115(NAO, NAO, NAO))
    !$omp single
    tau115 = 0.0
    !$omp end single

    !$omp do schedule(static)
    do r=1, NAO
        do q=1, NAO
            do p=1, NAO
    
                tau115(p, q, r) = tau115(p, q, r) + ( &
                    4 * t2(q, p) * tau109(r, p, q)&
                )
    
            end do
        end do
    end do
    !$omp end do

    deallocate(tau109)

    allocate(tau110(NAO, NAO, NAO))
    !$omp single
    tau110 = 0.0
    !$omp end single

    !$omp do schedule(static)
    do r=1, NAO
        do q=1, NAO
            do p=1, NAO
    
                tau110(p, q, r) = tau110(p, q, r) + ( &
                    t1(q) * t3(p, p, r)&
                )
    
            end do
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do r=1, NAO
        do q=1, NAO
            do p=1, NAO
    
                tau110(p, q, r) = tau110(p, q, r) + ( &
                    2 * t1(p) * t3(r, p, q)&
                )
    
            end do
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do r=1, NAO
        do q=1, NAO
            do p=1, NAO
    
                tau110(p, q, r) = tau110(p, q, r) + ( &
                    t1(p)**2 * t2(r, q)&
                )
    
            end do
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do r=1, NAO
        do q=1, NAO
            do p=1, NAO
    
                tau112(p, q, r) = tau112(p, q, r) + ( &
                    2 * t1(q) * tau110(p, r, q)&
                )
    
            end do
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do r=1, NAO
        do q=1, NAO
            do p=1, NAO
    
                tau114(p, q, r) = tau114(p, q, r) + ( &
                    2 * t1(q) * tau110(p, r, q)&
                )
    
            end do
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do r=1, NAO
        do q=1, NAO
            do p=1, NAO
    
                tau115(p, q, r) = tau115(p, q, r) + ( &
                    2 * t1(q) * tau110(p, r, q)&
                )
    
            end do
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do r=1, NAO
        do q=1, NAO
            do p=1, NAO
    
                Res3(p, q, r) = Res3(p, q, r) - ( &
                    2 * tau62(q, r) * tau110(r, q, p)&
                )
    
            end do
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do r=1, NAO
        do q=1, NAO
            do p=1, NAO
    
                Res3(p, q, r) = Res3(p, q, r) - ( &
                    2 * tau62(r, q) * tau110(q, r, p)&
                )
    
            end do
        end do
    end do
    !$omp end do

    deallocate(tau110)

    allocate(tau111(NAO, NAO))
    !$omp single
    tau111 = 0.0
    !$omp end single

    !$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
    
            tau111(p, q) = tau111(p, q) + ( &
                2 * t2(q, p)**2&
            )
    
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
    
            tau111(p, q) = tau111(p, q) + ( &
                t1(p)**2 * t1(q)**2&
            )
    
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do r=1, NAO
        do q=1, NAO
            do p=1, NAO
    
                tau112(p, q, r) = tau112(p, q, r) + ( &
                    t1(r) * tau111(q, p)&
                )
    
            end do
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do r=1, NAO
        do q=1, NAO
            do p=1, NAO
    
                tau115(p, q, r) = tau115(p, q, r) + ( &
                    t1(r) * tau111(q, p)&
                )
    
            end do
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do r=1, NAO
        do q=1, NAO
            do p=1, NAO
    
                tau112(p, q, r) = tau112(p, q, r) + ( &
                    2 * t2(r, q) * t3(p, p, q)&
                )
    
            end do
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do r=1, NAO
        do q=1, NAO
            do p=1, NAO
    
                tau112(p, q, r) = tau112(p, q, r) + ( &
                    2 * t1(q)**2 * t1(p) * t2(r, p)&
                )
    
            end do
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do r=1, NAO
        do q=1, NAO
            do p=1, NAO
    
                tau112(p, q, r) = tau112(p, q, r) + ( &
                    t1(p)**2 * t3(q, q, r)&
                )
    
            end do
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do r=1, NAO
        do q=1, NAO
            do p=1, NAO
    
                tau112(p, q, r) = tau112(p, q, r) + ( &
                    t1(q)**2 * t3(p, p, r)&
                )
    
            end do
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do r=1, NAO
        do q=1, NAO
            do p=1, NAO
    
                Res3(p, q, r) = Res3(p, q, r) + ( &
                    4 * H012(p, r, q) * tau112(r, q, p)&
                )
    
            end do
        end do
    end do
    !$omp end do

    deallocate(tau112)

    allocate(tau113(NAO, NAO))
    !$omp single
    tau113 = 0.0
    !$omp end single

    !$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
    
            tau113(p, q) = tau113(p, q) + ( &
                t3(p, p, q)&
            )
    
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
    
            tau113(p, q) = tau113(p, q) + ( &
                2 * t1(p) * t2(q, p)&
            )
    
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
    
            tau113(p, q) = tau113(p, q) + ( &
                t1(p)**2 * t1(q)&
            )
    
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do r=1, NAO
        do q=1, NAO
            do p=1, NAO
    
                tau114(p, q, r) = tau114(p, q, r) + ( &
                    t1(q)**2 * tau113(p, r)&
                )
    
            end do
        end do
    end do
    !$omp end do

    deallocate(tau113)

    !$omp do schedule(static)
    do r=1, NAO
        do q=1, NAO
            do p=1, NAO
    
                tau114(p, q, r) = tau114(p, q, r) + ( &
                    2 * t2(r, q) * t3(p, p, q)&
                )
    
            end do
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do r=1, NAO
        do q=1, NAO
            do p=1, NAO
    
                tau114(p, q, r) = tau114(p, q, r) + ( &
                    2 * t2(q, p)**2 * t1(r)&
                )
    
            end do
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do r=1, NAO
        do q=1, NAO
            do p=1, NAO
    
                Res3(p, q, r) = Res3(p, q, r) + ( &
                    4 * H012(q, r, p) * tau114(r, p, q)&
                )
    
            end do
        end do
    end do
    !$omp end do

    deallocate(tau114)

    !$omp do schedule(static)
    do r=1, NAO
        do q=1, NAO
            do p=1, NAO
    
                tau115(p, q, r) = tau115(p, q, r) + ( &
                    2 * t2(r, q) * t3(p, p, q)&
                )
    
            end do
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do r=1, NAO
        do q=1, NAO
            do p=1, NAO
    
                tau115(p, q, r) = tau115(p, q, r) + ( &
                    2 * t1(q)**2 * t1(p) * t2(r, p)&
                )
    
            end do
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do r=1, NAO
        do q=1, NAO
            do p=1, NAO
    
                Res3(p, q, r) = Res3(p, q, r) + ( &
                    4 * H012(r, q, p) * tau115(q, p, r)&
                )
    
            end do
        end do
    end do
    !$omp end do

    deallocate(tau115)

    allocate(tau116(NAO, NAO, NAO))
    !$omp single
    tau116 = 0.0
    !$omp end single

    !$omp do schedule(static)
    do r=1, NAO
        do q=1, NAO
            do p=1, NAO
    
                tau116(p, q, r) = tau116(p, q, r) + ( &
                    t3(r, p, q)&
                )
    
            end do
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do r=1, NAO
        do q=1, NAO
            do p=1, NAO
    
                tau116(p, q, r) = tau116(p, q, r) + ( &
                    t1(p) * t2(r, q)&
                )
    
            end do
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do r=1, NAO
        do q=1, NAO
            do p=1, NAO
    
                tau116(p, q, r) = tau116(p, q, r) + ( &
                    t1(r) * t2(q, p)&
                )
    
            end do
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do r=1, NAO
        do q=1, NAO
            do p=1, NAO
    
                tau117(p, q, r) = tau117(p, q, r) + ( &
                    2 * t1(p) * tau116(r, p, q)&
                )
    
            end do
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do r=1, NAO
        do q=1, NAO
            do p=1, NAO
    
                tau118(p, q, r) = tau118(p, q, r) + ( &
                    2 * t1(p) * tau116(r, p, q)&
                )
    
            end do
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do r=1, NAO
        do q=1, NAO
            do p=1, NAO
    
                tau119(p, q, r) = tau119(p, q, r) + ( &
                    2 * t1(p) * tau116(r, p, q)&
                )
    
            end do
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do r=1, NAO
        do q=1, NAO
            do p=1, NAO
    
                tau117(p, q, r) = tau117(p, q, r) + ( &
                    2 * t2(p, q) * t2(p, r)&
                )
    
            end do
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do r=1, NAO
        do q=1, NAO
            do p=1, NAO
    
                tau117(p, q, r) = tau117(p, q, r) + ( &
                    t1(q) * t3(p, p, r)&
                )
    
            end do
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do r=1, NAO
        do q=1, NAO
            do p=1, NAO
    
                tau117(p, q, r) = tau117(p, q, r) + ( &
                    t1(r) * t3(p, p, q)&
                )
    
            end do
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do r=1, NAO
        do q=1, NAO
            do p=1, NAO
    
                Res3(p, q, r) = Res3(p, q, r) - ( &
                    8 * H021(q, p, r) * tau117(r, q, p)&
                )
    
            end do
        end do
    end do
    !$omp end do

    deallocate(tau117)

    !$omp do schedule(static)
    do r=1, NAO
        do q=1, NAO
            do p=1, NAO
    
                tau118(p, q, r) = tau118(p, q, r) + ( &
                    t1(p)**2 * t2(r, q)&
                )
    
            end do
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do r=1, NAO
        do q=1, NAO
            do p=1, NAO
    
                tau118(p, q, r) = tau118(p, q, r) + ( &
                    2 * t2(p, q) * t2(p, r)&
                )
    
            end do
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do r=1, NAO
        do q=1, NAO
            do p=1, NAO
    
                Res3(p, q, r) = Res3(p, q, r) - ( &
                    8 * H021(r, p, q) * tau118(q, r, p)&
                )
    
            end do
        end do
    end do
    !$omp end do

    deallocate(tau118)

    !$omp do schedule(static)
    do r=1, NAO
        do q=1, NAO
            do p=1, NAO
    
                tau119(p, q, r) = tau119(p, q, r) + ( &
                    2 * t2(p, q) * t2(p, r)&
                )
    
            end do
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do r=1, NAO
        do q=1, NAO
            do p=1, NAO
    
                Res3(p, q, r) = Res3(p, q, r) - ( &
                    8 * H021(r, q, p) * tau119(p, r, q)&
                )
    
            end do
        end do
    end do
    !$omp end do

    deallocate(tau119)

    allocate(tau120(NAO, NAO, NAO))
    !$omp single
    tau120 = 0.0
    !$omp end single

    !$omp do schedule(static)
    do r=1, NAO
        do q=1, NAO
            do p=1, NAO
                do s=1, NAO
                    tau120(p, q, r) = tau120(p, q, r) + ( &
                        t2(r, s) * H021(p, q, s)&
                    )
                end do
            end do
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do r=1, NAO
        do q=1, NAO
            do p=1, NAO
    
                tau122(p, q, r) = tau122(p, q, r) + ( &
                    2 * tau120(q, p, r)&
                )
    
            end do
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do r=1, NAO
        do q=1, NAO
            do p=1, NAO
    
                tau123(p, q, r) = tau123(p, q, r) - ( &
                    2 * tau120(q, p, r)&
                )
    
            end do
        end do
    end do
    !$omp end do

    deallocate(tau120)

    allocate(tau121(NAO, NAO, NAO))
    !$omp single
    tau121 = 0.0
    !$omp end single

    !$omp do schedule(static)
    do r=1, NAO
        do q=1, NAO
            do p=1, NAO
                do s=1, NAO
                    tau121(p, q, r) = tau121(p, q, r) + ( &
                        H012(p, q, s) * t3(r, s, q)&
                    )
                end do
            end do
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do r=1, NAO
        do q=1, NAO
            do p=1, NAO
    
                tau122(p, q, r) = tau122(p, q, r) - ( &
                    2 * tau121(p, q, r)&
                )
    
            end do
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do r=1, NAO
        do q=1, NAO
            do p=1, NAO
    
                tau122(p, q, r) = tau122(p, q, r) - ( &
                    2 * tau121(q, p, r)&
                )
    
            end do
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do r=1, NAO
        do q=1, NAO
            do p=1, NAO
    
                tau123(p, q, r) = tau123(p, q, r) + ( &
                    2 * tau121(p, q, r)&
                )
    
            end do
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do r=1, NAO
        do q=1, NAO
            do p=1, NAO
    
                tau123(p, q, r) = tau123(p, q, r) + ( &
                    2 * tau121(q, p, r)&
                )
    
            end do
        end do
    end do
    !$omp end do

    deallocate(tau121)

    !$omp do schedule(static)
    do r=1, NAO
        do q=1, NAO
            do p=1, NAO
    
                tau122(p, q, r) = tau122(p, q, r) + ( &
                    12 * t1(r) * H030(q, r, p)&
                )
    
            end do
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do r=1, NAO
        do q=1, NAO
            do p=1, NAO
    
                tau122(p, q, r) = tau122(p, q, r) + ( &
                    H120(r, p, q)&
                )
    
            end do
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do r=1, NAO
        do q=1, NAO
            do p=1, NAO
    
                tau122(p, q, r) = tau122(p, q, r) + ( &
                    H120(r, q, p)&
                )
    
            end do
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do r=1, NAO
        do q=1, NAO
            do p=1, NAO
    
                Res3(p, q, r) = Res3(p, q, r) + ( &
                    4 * tau7(r, q) * tau122(r, q, p)&
                )
    
            end do
        end do
    end do
    !$omp end do

    deallocate(tau122)

    !$omp do schedule(static)
    do r=1, NAO
        do q=1, NAO
            do p=1, NAO
    
                tau123(p, q, r) = tau123(p, q, r) - ( &
                    H120(r, p, q)&
                )
    
            end do
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do r=1, NAO
        do q=1, NAO
            do p=1, NAO
    
                tau123(p, q, r) = tau123(p, q, r) - ( &
                    H120(r, q, p)&
                )
    
            end do
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do r=1, NAO
        do q=1, NAO
            do p=1, NAO
    
                Res3(p, q, r) = Res3(p, q, r) - ( &
                    4 * tau7(r, p) * tau123(r, p, q)&
                )
    
            end do
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do r=1, NAO
        do q=1, NAO
            do p=1, NAO
    
                Res3(p, q, r) = Res3(p, q, r) - ( &
                    4 * tau7(q, p) * tau123(q, p, r)&
                )
    
            end do
        end do
    end do
    !$omp end do

    deallocate(tau123)

    deallocate(tau7)

    !$omp do schedule(static)
    do r=1, NAO
        do q=1, NAO
            do p=1, NAO
    
                tau124(p, q, r) = tau124(p, q, r) + ( &
                    2 * t1(p)**2 * t1(r) * t2(q, r)&
                )
    
            end do
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do r=1, NAO
        do q=1, NAO
            do p=1, NAO
    
                tau124(p, q, r) = tau124(p, q, r) + ( &
                    t1(p)**2 * t3(r, r, q)&
                )
    
            end do
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do r=1, NAO
        do q=1, NAO
            do p=1, NAO
    
                tau124(p, q, r) = tau124(p, q, r) + ( &
                    t1(r)**2 * t3(p, p, q)&
                )
    
            end do
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do r=1, NAO
        do q=1, NAO
            do p=1, NAO
    
                Res3(p, q, r) = Res3(p, q, r) + ( &
                    2 * tau57(r, q) * tau124(q, p, r)&
                )
    
            end do
        end do
    end do
    !$omp end do

    deallocate(tau57)

    deallocate(tau124)

    !$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
    
            tau126(p, q) = tau126(p, q) + ( &
                3 * H030(p, p, q)&
            )
    
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
    
            tau127(p, q) = tau127(p, q) + ( &
                tau126(p, q)&
            )
    
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
    
            tau127(p, q) = tau127(p, q) + ( &
                tau126(q, p)&
            )
    
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
    
            tau128(p, q) = tau128(p, q) + ( &
                tau126(p, q)&
            )
    
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
    
            tau128(p, q) = tau128(p, q) + ( &
                tau126(q, p)&
            )
    
        end do
    end do
    !$omp end do

    deallocate(tau126)

    !$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
    
            tau127(p, q) = tau127(p, q) + ( &
                H020(q, p)&
            )
    
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do r=1, NAO
        do q=1, NAO
            do p=1, NAO
    
                Res3(p, q, r) = Res3(p, q, r) + ( &
                    8 * tau127(q, p) * tau103(q, r, p)&
                )
    
            end do
        end do
    end do
    !$omp end do

    deallocate(tau127)

    deallocate(tau103)

    !$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
    
            tau128(p, q) = tau128(p, q) + ( &
                H020(q, p)&
            )
    
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do r=1, NAO
        do q=1, NAO
            do p=1, NAO
    
                Res3(p, q, r) = Res3(p, q, r) + ( &
                    8 * tau128(r, q) * tau116(r, p, q)&
                )
    
            end do
        end do
    end do
    !$omp end do

    deallocate(tau116)

    allocate(tau129(NAO, NAO, NAO))
    !$omp single
    tau129 = 0.0
    !$omp end single

    !$omp do schedule(static)
    do r=1, NAO
        do q=1, NAO
            do p=1, NAO
    
                tau129(p, q, r) = tau129(p, q, r) + ( &
                    t3(r, p, q)&
                )
    
            end do
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do r=1, NAO
        do q=1, NAO
            do p=1, NAO
    
                tau129(p, q, r) = tau129(p, q, r) + ( &
                    t1(p) * t2(r, q)&
                )
    
            end do
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do r=1, NAO
        do q=1, NAO
            do p=1, NAO
    
                tau129(p, q, r) = tau129(p, q, r) + ( &
                    t1(q) * t2(r, p)&
                )
    
            end do
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do r=1, NAO
        do q=1, NAO
            do p=1, NAO
    
                Res3(p, q, r) = Res3(p, q, r) + ( &
                    8 * tau128(r, p) * tau129(r, p, q)&
                )
    
            end do
        end do
    end do
    !$omp end do

    deallocate(tau128)

    deallocate(tau129)

    !$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
    
            tau130(p, q) = tau130(p, q) - ( &
                2 * H020(q, p)&
            )
    
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do r=1, NAO
        do q=1, NAO
            do p=1, NAO
    
                Res3(p, q, r) = Res3(p, q, r) - ( &
                    4 * tau130(q, p) * t3(r, p, q)&
                )
    
            end do
        end do
    end do
    !$omp end do

    deallocate(tau130)

    allocate(tau131(NAO, NAO, NAO))
    !$omp single
    tau131 = 0.0
    !$omp end single

    !$omp do schedule(static)
    do r=1, NAO
        do q=1, NAO
            do p=1, NAO
    
                tau131(p, q, r) = tau131(p, q, r) + ( &
                    t1(r) * t3(p, p, q)&
                )
    
            end do
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do r=1, NAO
        do q=1, NAO
            do p=1, NAO
    
                tau131(p, q, r) = tau131(p, q, r) + ( &
                    2 * t1(p) * t3(r, p, q)&
                )
    
            end do
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do r=1, NAO
        do q=1, NAO
            do p=1, NAO
    
                tau131(p, q, r) = tau131(p, q, r) + ( &
                    t1(p)**2 * t2(r, q)&
                )
    
            end do
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do r=1, NAO
        do q=1, NAO
            do p=1, NAO
    
                Res3(p, q, r) = Res3(p, q, r) - ( &
                    2 * tau62(p, r) * tau131(r, q, p)&
                )
    
            end do
        end do
    end do
    !$omp end do

    deallocate(tau131)

    !$omp do schedule(static)
    do r=1, NAO
        do q=1, NAO
            do p=1, NAO
    
                tau132(p, q, r) = tau132(p, q, r) - ( &
                    H111(p, q, r)&
                )
    
            end do
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do r=1, NAO
        do q=1, NAO
            do p=1, NAO
    
                Res3(p, q, r) = Res3(p, q, r) + ( &
                    2 * tau104(q, p) * tau132(r, p, q)&
                )
    
            end do
        end do
    end do
    !$omp end do

    deallocate(tau132)

    allocate(tau133(NAO, NAO, NAO))
    !$omp single
    tau133 = 0.0
    !$omp end single

    !$omp do schedule(static)
    do r=1, NAO
        do q=1, NAO
            do p=1, NAO
    
                tau133(p, q, r) = tau133(p, q, r) + ( &
                    2 * t1(p) * t3(r, p, q)&
                )
    
            end do
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do r=1, NAO
        do q=1, NAO
            do p=1, NAO
    
                tau133(p, q, r) = tau133(p, q, r) + ( &
                    t1(p)**2 * t2(r, q)&
                )
    
            end do
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do r=1, NAO
        do q=1, NAO
            do p=1, NAO
    
                Res3(p, q, r) = Res3(p, q, r) - ( &
                    2 * tau62(r, p) * tau133(p, r, q)&
                )
    
            end do
        end do
    end do
    !$omp end do

    deallocate(tau133)

    deallocate(tau62)

    !$omp do schedule(static)
    do r=1, NAO
        do q=1, NAO
            do p=1, NAO
    
                tau134(p, q, r) = tau134(p, q, r) + ( &
                    H102(p, r, q)&
                )
    
            end do
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do r=1, NAO
        do q=1, NAO
            do p=1, NAO
    
                tau135(p, q, r) = tau135(p, q, r) + ( &
                    2 * t1(q) * tau134(p, r, q)&
                )
    
            end do
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do r=1, NAO
        do q=1, NAO
            do p=1, NAO
    
                tau137(p, q, r) = tau137(p, q, r) + ( &
                    2 * t1(q) * t1(r) * tau134(p, r, q)&
                )
    
            end do
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do r=1, NAO
        do q=1, NAO
            do p=1, NAO
    
                Res3(p, q, r) = Res3(p, q, r) + ( &
                    2 * tau107(p, r) * tau134(q, r, p)&
                )
    
            end do
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do r=1, NAO
        do q=1, NAO
            do p=1, NAO
    
                Res3(p, q, r) = Res3(p, q, r) + ( &
                    2 * tau107(p, q) * tau134(r, q, p)&
                )
    
            end do
        end do
    end do
    !$omp end do

    deallocate(tau107)

    !$omp do schedule(static)
    do r=1, NAO
        do q=1, NAO
            do p=1, NAO
    
                Res3(p, q, r) = Res3(p, q, r) + ( &
                    2 * tau111(r, q) * tau134(p, r, q)&
                )
    
            end do
        end do
    end do
    !$omp end do

    deallocate(tau134)

    deallocate(tau111)

    !$omp do schedule(static)
    do r=1, NAO
        do q=1, NAO
            do p=1, NAO
    
                tau135(p, q, r) = tau135(p, q, r) - ( &
                    H111(p, q, r)&
                )
    
            end do
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do r=1, NAO
        do q=1, NAO
            do p=1, NAO
    
                Res3(p, q, r) = Res3(p, q, r) + ( &
                    2 * tau68(r, q) * tau135(p, q, r)&
                )
    
            end do
        end do
    end do
    !$omp end do

    deallocate(tau68)

    deallocate(tau135)

    !$omp do schedule(static)
    do r=1, NAO
        do q=1, NAO
            do p=1, NAO
    
                tau137(p, q, r) = tau137(p, q, r) + ( &
                    12 * t1(p) * H030(r, p, q)&
                )
    
            end do
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do r=1, NAO
        do q=1, NAO
            do p=1, NAO
    
                tau137(p, q, r) = tau137(p, q, r) - ( &
                    t1(r) * H111(p, q, r)&
                )
    
            end do
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do r=1, NAO
        do q=1, NAO
            do p=1, NAO
    
                tau137(p, q, r) = tau137(p, q, r) - ( &
                    t1(q) * H111(p, r, q)&
                )
    
            end do
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do r=1, NAO
        do q=1, NAO
            do p=1, NAO
    
                Res3(p, q, r) = Res3(p, q, r) + ( &
                    4 * t2(r, p) * tau137(q, r, p)&
                )
    
            end do
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do r=1, NAO
        do q=1, NAO
            do p=1, NAO
    
                Res3(p, q, r) = Res3(p, q, r) + ( &
                    4 * t2(q, p) * tau137(r, q, p)&
                )
    
            end do
        end do
    end do
    !$omp end do

    deallocate(tau137)

    !$omp do schedule(static)
    do r=1, NAO
        do q=1, NAO
            do p=1, NAO
    
                tau138(p, q, r) = tau138(p, q, r) + ( &
                    H111(p, q, r)&
                )
    
            end do
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do r=1, NAO
        do q=1, NAO
            do p=1, NAO
    
                Res3(p, q, r) = Res3(p, q, r) - ( &
                    2 * tau104(r, p) * tau138(q, p, r)&
                )
    
            end do
        end do
    end do
    !$omp end do

    deallocate(tau104)

    !$omp do schedule(static)
    do r=1, NAO
        do q=1, NAO
            do p=1, NAO
    
                Res3(p, q, r) = Res3(p, q, r) - ( &
                    2 * t1(r)**2 * t1(q) * tau138(p, q, r)&
                )
    
            end do
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do r=1, NAO
        do q=1, NAO
            do p=1, NAO
    
                Res3(p, q, r) = Res3(p, q, r) - ( &
                    2 * t1(p)**2 * t1(r) * tau138(q, r, p)&
                )
    
            end do
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do r=1, NAO
        do q=1, NAO
            do p=1, NAO
    
                Res3(p, q, r) = Res3(p, q, r) - ( &
                    2 * t1(p)**2 * t1(q) * tau138(r, q, p)&
                )
    
            end do
        end do
    end do
    !$omp end do

    allocate(tau139(NAO, NAO))
    !$omp single
    tau139 = 0.0
    !$omp end single

    !$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
    
            tau139(p, q) = tau139(p, q) + ( &
                2 * t1(p) * t2(q, p)&
            )
    
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
    
            tau139(p, q) = tau139(p, q) + ( &
                t1(p)**2 * t1(q)&
            )
    
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do r=1, NAO
        do q=1, NAO
            do p=1, NAO
    
                Res3(p, q, r) = Res3(p, q, r) - ( &
                    2 * tau139(q, r) * tau138(p, r, q)&
                )
    
            end do
        end do
    end do
    !$omp end do

    deallocate(tau138)

    deallocate(tau139)

    !$omp do schedule(static)
    do r=1, NAO
        do q=1, NAO
            do p=1, NAO
    
                tau140(p, q, r) = tau140(p, q, r) + ( &
                    H111(r, p, q)&
                )
    
            end do
        end do
    end do
    !$omp end do

    allocate(tau141(NAO, NAO, NAO))
    !$omp single
    tau141 = 0.0
    !$omp end single

    !$omp do schedule(static)
    do r=1, NAO
        do q=1, NAO
            do p=1, NAO
                do s=1, NAO
                    tau141(p, q, r) = tau141(p, q, r) + ( &
                        t3(s, p, q) * tau140(p, s, r)&
                    )
                end do
            end do
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do r=1, NAO
        do q=1, NAO
            do p=1, NAO
    
                Res3(p, q, r) = Res3(p, q, r) + ( &
                    2 * tau141(p, r, q)&
                )
    
            end do
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do r=1, NAO
        do q=1, NAO
            do p=1, NAO
    
                Res3(p, q, r) = Res3(p, q, r) + ( &
                    2 * tau141(q, r, p)&
                )
    
            end do
        end do
    end do
    !$omp end do

    deallocate(tau141)

    !$omp do schedule(static)
    do r=1, NAO
        do q=1, NAO
            do p=1, NAO
                do s=1, NAO
                    Res3(p, q, r) = Res3(p, q, r) + ( &
                        2 * t3(s, q, r) * tau140(r, s, p)&
                    )
                end do
            end do
        end do
    end do
    !$omp end do

    deallocate(tau140)

    !$omp do schedule(static)
    do r=1, NAO
        do q=1, NAO
            do p=1, NAO
    
                tau142(p, q, r) = tau142(p, q, r) + ( &
                    H111(r, p, q)&
                )
    
            end do
        end do
    end do
    !$omp end do

    allocate(tau143(NAO, NAO, NAO))
    !$omp single
    tau143 = 0.0
    !$omp end single

    !$omp do schedule(static)
    do r=1, NAO
        do q=1, NAO
            do p=1, NAO
                do s=1, NAO
                    tau143(p, q, r) = tau143(p, q, r) + ( &
                        t3(s, p, q) * tau142(q, s, r)&
                    )
                end do
            end do
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do r=1, NAO
        do q=1, NAO
            do p=1, NAO
    
                Res3(p, q, r) = Res3(p, q, r) + ( &
                    2 * tau143(p, q, r)&
                )
    
            end do
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do r=1, NAO
        do q=1, NAO
            do p=1, NAO
    
                Res3(p, q, r) = Res3(p, q, r) + ( &
                    2 * tau143(p, r, q)&
                )
    
            end do
        end do
    end do
    !$omp end do

    deallocate(tau143)

    !$omp do schedule(static)
    do r=1, NAO
        do q=1, NAO
            do p=1, NAO
                do s=1, NAO
                    Res3(p, q, r) = Res3(p, q, r) + ( &
                        2 * t3(s, p, q) * tau142(p, s, r)&
                    )
                end do
            end do
        end do
    end do
    !$omp end do

    deallocate(tau142)

    !$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
    
            tau145(p, q) = tau145(p, q) + ( &
                H101(p, q)&
            )
    
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
    
            tau145(p, q) = tau145(p, q) + ( &
                2 * H111(p, q, q)&
            )
    
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
    
            tau146(p, q) = tau146(p, q) + ( &
                t1(q) * tau145(p, q)&
            )
    
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
    
            tau147(p, q) = tau147(p, q) + ( &
                t1(p) * tau145(q, p)&
            )
    
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
    
            tau148(p, q) = tau148(p, q) + ( &
                t1(p) * tau145(q, p)&
            )
    
        end do
    end do
    !$omp end do

    deallocate(tau145)

    !$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
    
            tau146(p, q) = tau146(p, q) - ( &
                H110(p, q)&
            )
    
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
    
            tau146(p, q) = tau146(p, q) - ( &
                2 * H120(p, q, q)&
            )
    
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
    
            tau146(p, q) = tau146(p, q) - ( &
                3 * t1(q)**2 * H102(p, q, q)&
            )
    
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do r=1, NAO
        do q=1, NAO
            do p=1, NAO
    
                Res3(p, q, r) = Res3(p, q, r) - ( &
                    2 * t2(r, q) * tau146(p, q)&
                )
    
            end do
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do r=1, NAO
        do q=1, NAO
            do p=1, NAO
    
                Res3(p, q, r) = Res3(p, q, r) - ( &
                    2 * t2(r, p) * tau146(q, p)&
                )
    
            end do
        end do
    end do
    !$omp end do

    deallocate(tau146)

    !$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
    
            tau147(p, q) = tau147(p, q) - ( &
                H110(q, p)&
            )
    
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
    
            tau147(p, q) = tau147(p, q) - ( &
                2 * H120(q, p, p)&
            )
    
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
    
            tau147(p, q) = tau147(p, q) - ( &
                3 * t1(p)**2 * H102(q, p, p)&
            )
    
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do r=1, NAO
        do q=1, NAO
            do p=1, NAO
    
                Res3(p, q, r) = Res3(p, q, r) - ( &
                    2 * t2(r, q) * tau147(r, p)&
                )
    
            end do
        end do
    end do
    !$omp end do

    deallocate(tau147)

    !$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
    
            tau148(p, q) = tau148(p, q) - ( &
                H110(q, p)&
            )
    
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
    
            tau148(p, q) = tau148(p, q) - ( &
                2 * H120(q, p, p)&
            )
    
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
    
            tau148(p, q) = tau148(p, q) - ( &
                3 * t1(p)**2 * H102(q, p, p)&
            )
    
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do r=1, NAO
        do q=1, NAO
            do p=1, NAO
    
                Res3(p, q, r) = Res3(p, q, r) - ( &
                    2 * t2(q, p) * tau148(q, r)&
                )
    
            end do
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do r=1, NAO
        do q=1, NAO
            do p=1, NAO
    
                Res3(p, q, r) = Res3(p, q, r) - ( &
                    2 * t2(r, p) * tau148(r, q)&
                )
    
            end do
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do r=1, NAO
        do q=1, NAO
            do p=1, NAO
    
                Res3(p, q, r) = Res3(p, q, r) - ( &
                    2 * t2(q, p) * tau148(p, r)&
                )
    
            end do
        end do
    end do
    !$omp end do

    deallocate(tau148)

    !$omp do schedule(static)
    do p=1, NAO
    
        tau149(p) = tau149(p) - ( &
            H010(p)&
        )
    
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
    
        tau149(p) = tau149(p) - ( &
            2 * H020(p, p)&
        )
    
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
    
        tau149(p) = tau149(p) - ( &
            4 * H030(p, p, p)&
        )
    
    end do
    !$omp end do

    !$omp do schedule(static)
    do r=1, NAO
        do q=1, NAO
            do p=1, NAO
    
                Res3(p, q, r) = Res3(p, q, r) - ( &
                    2 * tau149(p) * t3(r, p, q)&
                )
    
            end do
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do r=1, NAO
        do q=1, NAO
            do p=1, NAO
    
                Res3(p, q, r) = Res3(p, q, r) - ( &
                    2 * tau149(q) * t3(r, p, q)&
                )
    
            end do
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do r=1, NAO
        do q=1, NAO
            do p=1, NAO
    
                Res3(p, q, r) = Res3(p, q, r) - ( &
                    2 * tau149(r) * t3(r, p, q)&
                )
    
            end do
        end do
    end do
    !$omp end do

    deallocate(tau149)

    !$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
    
            tau150(p, q) = tau150(p, q) + ( &
                H101(q, p)&
            )
    
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
    
            tau150(p, q) = tau150(p, q) + ( &
                2 * H111(q, p, p)&
            )
    
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
    
            tau150(p, q) = tau150(p, q) - ( &
                6 * t1(p) * H102(q, p, p)&
            )
    
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do r=1, NAO
        do q=1, NAO
            do p=1, NAO
    
                Res3(p, q, r) = Res3(p, q, r) - ( &
                    tau150(q, r) * t3(q, q, p)&
                )
    
            end do
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do r=1, NAO
        do q=1, NAO
            do p=1, NAO
    
                Res3(p, q, r) = Res3(p, q, r) - ( &
                    tau150(r, p) * t3(r, r, q)&
                )
    
            end do
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do r=1, NAO
        do q=1, NAO
            do p=1, NAO
    
                Res3(p, q, r) = Res3(p, q, r) - ( &
                    tau150(r, q) * t3(r, r, p)&
                )
    
            end do
        end do
    end do
    !$omp end do

    deallocate(tau150)

    allocate(tau151(NAO, NAO, NAO))
    !$omp single
    tau151 = 0.0
    !$omp end single

    !$omp do schedule(static)
    do r=1, NAO
        do q=1, NAO
            do p=1, NAO
    
                tau151(p, q, r) = tau151(p, q, r) + ( &
                    H201(p, q, r)&
                )
    
            end do
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do r=1, NAO
        do q=1, NAO
            do p=1, NAO
    
                tau151(p, q, r) = tau151(p, q, r) + ( &
                    H201(q, p, r)&
                )
    
            end do
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do r=1, NAO
        do q=1, NAO
            do p=1, NAO
    
                Res3(p, q, r) = Res3(p, q, r) - ( &
                    t1(p)**2 * tau151(r, q, p)&
                )
    
            end do
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do r=1, NAO
        do q=1, NAO
            do p=1, NAO
    
                Res3(p, q, r) = Res3(p, q, r) - ( &
                    t1(q)**2 * tau151(p, r, q)&
                )
    
            end do
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do r=1, NAO
        do q=1, NAO
            do p=1, NAO
    
                Res3(p, q, r) = Res3(p, q, r) - ( &
                    t1(r)**2 * tau151(q, p, r)&
                )
    
            end do
        end do
    end do
    !$omp end do

    deallocate(tau151)

    !$omp do schedule(static)
    do r=1, NAO
        do q=1, NAO
            do p=1, NAO
    
                tau152(p, q, r) = tau152(p, q, r) + ( &
                    H201(q, r, p)&
                )
    
            end do
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do r=1, NAO
        do q=1, NAO
            do p=1, NAO
    
                tau152(p, q, r) = tau152(p, q, r) + ( &
                    H201(r, q, p)&
                )
    
            end do
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do r=1, NAO
        do q=1, NAO
            do p=1, NAO
                do s=1, NAO
                    Res3(p, q, r) = Res3(p, q, r) + ( &
                        t2(s, p) * tau152(s, r, q)&
                    )
                end do
            end do
        end do
    end do
    !$omp end do

    deallocate(tau152)

    !$omp do schedule(static) reduction(+:Ene)
    
    do q=1, NAO
        do p=1, NAO
            Ene = Ene + ( &
                H002(q, p) * t2(q, p)&
            )
        end do
    end do
    
    !$omp end do

    !$omp do schedule(static) reduction(+:Ene)
    
    do r=1, NAO
        do q=1, NAO
            do p=1, NAO
                Ene = Ene + ( &
                    H003(r, p, q) * t3(r, p, q)&
                )
            end do
        end do
    end do
    
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            Res1(p) = Res1(p) + ( &
                t1(q) * H101(p, q)&
            )
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            Res1(p) = Res1(p) - ( &
                t1(q)**2 * H102(p, q, q)&
            )
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
    
        Res1(p) = Res1(p) + ( &
            H100(p)&
        )
    
    end do
    !$omp end do

    !$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
            do r=1, NAO
                Res2(p, q) = Res2(p, q) + ( &
                    H101(q, r) * t2(p, r)&
                )
            end do
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
            do r=1, NAO
                Res2(p, q) = Res2(p, q) + ( &
                    8 * H021(q, p, r) * t3(q, r, p)&
                )
            end do
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
    
            Res2(p, q) = Res2(p, q) + ( &
                2 * H200(q, p)&
            )
    
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
    
            Res2(p, q) = Res2(p, q) + ( &
                4 * t2(q, p)**2 * H002(q, p)&
            )
    
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do r=1, NAO
        do q=1, NAO
            do p=1, NAO
    
                Res3(p, q, r) = Res3(p, q, r) + ( &
                    6 * H300(r, p, q)&
                )
    
            end do
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do r=1, NAO
        do q=1, NAO
            do p=1, NAO
    
                Res3(p, q, r) = Res3(p, q, r) + ( &
                    48 * H030(r, p, q) * t3(r, p, q)&
                )
    
            end do
        end do
    end do
    !$omp end do

    !$omp end parallel

    End Subroutine CCSDT_SFS
End Module CCRes_SFS
