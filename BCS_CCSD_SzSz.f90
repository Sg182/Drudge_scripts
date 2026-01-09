    Module CCSzSz
    Use Precision
    Use Constants

    Contains

Subroutine CCSD_SzSz(SzSz,z1,z2,T1,T2,NAO,H20,H11,H02,H40,H31,H22,HT22,H13,H04)
    Implicit None
    Integer,           Intent(In)    :: NAO
    Complex (Kind=pr), Intent(In)    :: T1(NAO), T2(NAO,NAO)
    Complex (Kind=pr), Intent(In)    :: z1(NAO), z2(NAO,NAO)
    Complex (Kind=pr), Intent(In)    :: H20(NAO), H11(NAO), H02(NAO)
    Complex (Kind=pr), Intent(In)    :: H40(NAO,NAO), H31(NAO,NAO)
    Complex (Kind=pr), Intent(In)    :: H04(NAO,NAO), H13(NAO,NAO)
    Complex (Kind=pr), Intent(In)    :: H22(NAO,NAO), HT22(NAO,NAO)
    Complex (Kind=pr), Intent(Out)   :: SzSz(NAO,NAO) 
    Integer                          :: p, q, r, s, i, j, k, l

    real(Kind=pr) :: one(NAO)
    one = 1.0_pr
    complex(kind=pr) :: tau0

    complex(kind=pr) , dimension(:), allocatable :: tau1

    complex(kind=pr) , dimension(:), allocatable :: tau2

    complex(kind=pr) :: tau3

    complex(kind=pr) , dimension(:), allocatable :: tau4

    complex(kind=pr) , dimension(:), allocatable :: tau5

    complex(kind=pr) :: tau6

    complex(kind=pr) , dimension(:), allocatable :: tau7

    complex(kind=pr) , dimension(:), allocatable :: tau8

    complex(kind=pr) :: tau9

    complex(kind=pr) , dimension(:), allocatable :: tau10

    complex(kind=pr) :: tau11

    complex(kind=pr) , dimension(:), allocatable :: tau12

    complex(kind=pr) :: tau13

    complex(kind=pr) , dimension(:), allocatable :: tau14

    complex(kind=pr) , dimension(:), allocatable :: tau15

    complex(kind=pr) :: tau16

    complex(kind=pr) , dimension(:), allocatable :: tau17

    complex(kind=pr) :: tau18

    complex(kind=pr) , dimension(:), allocatable :: tau19

    complex(kind=pr) , dimension(:), allocatable :: tau20

    complex(kind=pr) :: tau21

    complex(kind=pr) , dimension(:), allocatable :: tau22

    complex(kind=pr) , dimension(:), allocatable :: tau23

    complex(kind=pr) :: tau24

    complex(kind=pr) , dimension(:), allocatable :: tau25

    complex(kind=pr) , dimension(:), allocatable :: tau26

    complex(kind=pr) :: tau27

    complex(kind=pr) , dimension(:), allocatable :: tau28

    complex(kind=pr) :: tau29

    complex(kind=pr) , dimension(:), allocatable :: tau30

    complex(kind=pr) , dimension(:), allocatable :: tau31

    complex(kind=pr) :: tau32

    complex(kind=pr) , dimension(:), allocatable :: tau33

    complex(kind=pr) , dimension(:), allocatable :: tau34

    complex(kind=pr) :: tau35

    complex(kind=pr) , dimension(:), allocatable :: tau36

    complex(kind=pr) , dimension(:), allocatable :: tau37

    complex(kind=pr) :: tau38

    complex(kind=pr) , dimension(:), allocatable :: tau39

    complex(kind=pr) , dimension(:), allocatable :: tau40

    complex(kind=pr) , dimension(:), allocatable :: tau41

    complex(kind=pr) :: tau42

    complex(kind=pr) , dimension(:), allocatable :: tau43

    complex(kind=pr) , dimension(:), allocatable :: tau44

    complex(kind=pr) :: tau45

    complex(kind=pr) , dimension(:), allocatable :: tau46

    complex(kind=pr) , dimension(:), allocatable :: tau47

    complex(kind=pr) , dimension(:, :), allocatable :: tau48

    complex(kind=pr) , dimension(:), allocatable :: tau49

    complex(kind=pr) , dimension(:), allocatable :: tau50

    complex(kind=pr) :: tau51

    complex(kind=pr) , dimension(:), allocatable :: tau52

    complex(kind=pr) , dimension(:), allocatable :: tau53

    complex(kind=pr) , dimension(:), allocatable :: tau54

    complex(kind=pr) , dimension(:), allocatable :: tau55

    complex(kind=pr) :: tau56

    complex(kind=pr) , dimension(:), allocatable :: tau57

    complex(kind=pr) , dimension(:), allocatable :: tau58

    complex(kind=pr) :: tau59

    complex(kind=pr) :: tau60

    complex(kind=pr) , dimension(:), allocatable :: tau61

    complex(kind=pr) :: tau62

    complex(kind=pr) , dimension(:), allocatable :: tau63

    complex(kind=pr) :: tau64

    complex(kind=pr) :: tau65

    complex(kind=pr) , dimension(:), allocatable :: tau66

    complex(kind=pr) , dimension(:), allocatable :: tau67

    complex(kind=pr) :: tau68

    complex(kind=pr) , dimension(:), allocatable :: tau69

    complex(kind=pr) , dimension(:), allocatable :: tau70

    complex(kind=pr) , dimension(:), allocatable :: tau71

    complex(kind=pr) :: tau72

    complex(kind=pr) , dimension(:), allocatable :: tau73

    complex(kind=pr) , dimension(:), allocatable :: tau74

    complex(kind=pr) :: tau75

    complex(kind=pr) , dimension(:), allocatable :: tau76

    complex(kind=pr) , dimension(:), allocatable :: tau77

    complex(kind=pr) , dimension(:), allocatable :: tau78

    complex(kind=pr) :: tau79

    complex(kind=pr) :: tau80

    complex(kind=pr) , dimension(:), allocatable :: tau81

    complex(kind=pr) , dimension(:), allocatable :: tau82

    complex(kind=pr) , dimension(:), allocatable :: tau83

    complex(kind=pr) :: tau84

    complex(kind=pr) :: tau85

    complex(kind=pr) , dimension(:), allocatable :: tau86

    complex(kind=pr) :: tau87

    complex(kind=pr) , dimension(:), allocatable :: tau88

    complex(kind=pr) , dimension(:), allocatable :: tau89

    complex(kind=pr) :: tau90

    complex(kind=pr) , dimension(:), allocatable :: tau91

    complex(kind=pr) , dimension(:), allocatable :: tau92

    complex(kind=pr) , dimension(:, :), allocatable :: tau93

    complex(kind=pr) , dimension(:), allocatable :: tau94

    complex(kind=pr) :: tau95

    complex(kind=pr) , dimension(:), allocatable :: tau96

    complex(kind=pr) :: tau97

    complex(kind=pr) , dimension(:), allocatable :: tau98

    complex(kind=pr) , dimension(:), allocatable :: tau99

    complex(kind=pr) :: tau100

    complex(kind=pr) , dimension(:), allocatable :: tau101

    complex(kind=pr) , dimension(:), allocatable :: tau102

    complex(kind=pr) :: tau103

    complex(kind=pr) , dimension(:), allocatable :: tau104

    complex(kind=pr) , dimension(:), allocatable :: tau105

    complex(kind=pr) , dimension(:), allocatable :: tau106

    complex(kind=pr) :: tau107

    complex(kind=pr) , dimension(:), allocatable :: tau108

    complex(kind=pr) :: tau109

    complex(kind=pr) , dimension(:), allocatable :: tau110

    complex(kind=pr) , dimension(:), allocatable :: tau111

    complex(kind=pr) , dimension(:), allocatable :: tau112

    complex(kind=pr) , dimension(:), allocatable :: tau113

    complex(kind=pr) :: tau114

    complex(kind=pr) , dimension(:), allocatable :: tau115

    complex(kind=pr) , dimension(:), allocatable :: tau116

    complex(kind=pr) , dimension(:), allocatable :: tau117

    complex(kind=pr) , dimension(:), allocatable :: tau118

    complex(kind=pr) :: tau119

    complex(kind=pr) , dimension(:), allocatable :: tau120

    complex(kind=pr) , dimension(:), allocatable :: tau121

    complex(kind=pr) :: tau122

    complex(kind=pr) , dimension(:), allocatable :: tau123

    complex(kind=pr) , dimension(:), allocatable :: tau124

    complex(kind=pr) , dimension(:), allocatable :: tau125

    complex(kind=pr) :: tau126

    complex(kind=pr) , dimension(:), allocatable :: tau127

    complex(kind=pr) , dimension(:), allocatable :: tau128

    complex(kind=pr) :: tau129

    complex(kind=pr) , dimension(:), allocatable :: tau130

    complex(kind=pr) , dimension(:), allocatable :: tau131

    complex(kind=pr) , dimension(:), allocatable :: tau132

    complex(kind=pr) :: tau133

    complex(kind=pr) , dimension(:), allocatable :: tau134

    complex(kind=pr) :: tau135

    complex(kind=pr) , dimension(:), allocatable :: tau136

    complex(kind=pr) , dimension(:), allocatable :: tau137

    complex(kind=pr) :: tau138

    complex(kind=pr) , dimension(:), allocatable :: tau139

    complex(kind=pr) , dimension(:), allocatable :: tau140

    complex(kind=pr) :: tau141

    complex(kind=pr) , dimension(:), allocatable :: tau142

    complex(kind=pr) :: tau143

    complex(kind=pr) , dimension(:), allocatable :: tau144

    complex(kind=pr) :: tau145

    complex(kind=pr) , dimension(:), allocatable :: tau146

    complex(kind=pr) , dimension(:), allocatable :: tau147

    complex(kind=pr) , dimension(:), allocatable :: tau148

    complex(kind=pr) :: tau149

    complex(kind=pr) , dimension(:), allocatable :: tau150

    complex(kind=pr) , dimension(:), allocatable :: tau151

    complex(kind=pr) , dimension(:), allocatable :: tau152

    complex(kind=pr) :: tau153

    complex(kind=pr) , dimension(:), allocatable :: tau154

    complex(kind=pr) :: tau155

    complex(kind=pr) , dimension(:), allocatable :: tau156

    complex(kind=pr) , dimension(:), allocatable :: tau157

    complex(kind=pr) , dimension(:), allocatable :: tau158

    complex(kind=pr) :: tau159

    complex(kind=pr) , dimension(:), allocatable :: tau160

    complex(kind=pr) , dimension(:), allocatable :: tau161

    complex(kind=pr) :: tau162

    complex(kind=pr) , dimension(:), allocatable :: tau163

    complex(kind=pr) , dimension(:), allocatable :: tau164

    complex(kind=pr) , dimension(:), allocatable :: tau165

    complex(kind=pr) :: tau166

    complex(kind=pr) , dimension(:), allocatable :: tau167

    complex(kind=pr) :: tau168

    complex(kind=pr) , dimension(:), allocatable :: tau169

    complex(kind=pr) , dimension(:), allocatable :: tau170

    complex(kind=pr) , dimension(:), allocatable :: tau171

    complex(kind=pr) :: tau172

    complex(kind=pr) , dimension(:), allocatable :: tau173

    complex(kind=pr) :: tau174

    complex(kind=pr) , dimension(:), allocatable :: tau175

    complex(kind=pr) , dimension(:, :), allocatable :: tau176

    complex(kind=pr) , dimension(:), allocatable :: tau177

    complex(kind=pr) , dimension(:), allocatable :: tau178

    complex(kind=pr) :: tau179

    complex(kind=pr) , dimension(:), allocatable :: tau180

    complex(kind=pr) :: tau181

    complex(kind=pr) , dimension(:), allocatable :: tau182

    complex(kind=pr) :: tau183

    complex(kind=pr) , dimension(:), allocatable :: tau184

    complex(kind=pr) , dimension(:), allocatable :: tau185

    complex(kind=pr) , dimension(:), allocatable :: tau186

    complex(kind=pr) , dimension(:), allocatable :: tau187

    complex(kind=pr) , dimension(:), allocatable :: tau188

    complex(kind=pr) :: tau189

    complex(kind=pr) , dimension(:), allocatable :: tau190

    complex(kind=pr) , dimension(:), allocatable :: tau191

    complex(kind=pr) , dimension(:), allocatable :: tau192

    complex(kind=pr) :: tau193

    complex(kind=pr) , dimension(:), allocatable :: tau194

    complex(kind=pr) :: tau195

    complex(kind=pr) , dimension(:), allocatable :: tau196

    complex(kind=pr) , dimension(:), allocatable :: tau197

    complex(kind=pr) , dimension(:), allocatable :: tau198

    complex(kind=pr) :: tau199

    complex(kind=pr) , dimension(:), allocatable :: tau200

    complex(kind=pr) , dimension(:), allocatable :: tau201

    complex(kind=pr) , dimension(:), allocatable :: tau202

    complex(kind=pr) :: tau203

    complex(kind=pr) , dimension(:), allocatable :: tau204

    complex(kind=pr) , dimension(:, :), allocatable :: tau205

    complex(kind=pr) , dimension(:, :), allocatable :: tau206

    complex(kind=pr) , dimension(:, :), allocatable :: tau207

    complex(kind=pr) , dimension(:, :), allocatable :: tau208

    complex(kind=pr) , dimension(:, :), allocatable :: tau209

    complex(kind=pr) , dimension(:, :), allocatable :: tau210

    complex(kind=pr) , dimension(:, :), allocatable :: tau211

    complex(kind=pr) , dimension(:, :), allocatable :: tau212

    complex(kind=pr) , dimension(:, :), allocatable :: tau213

    complex(kind=pr) , dimension(:, :), allocatable :: tau214

    complex(kind=pr) , dimension(:, :), allocatable :: tau215

    !$omp parallel default(shared)

    !$omp single
    tau0 = 0.0
    !$omp end single

    !$omp do schedule(static) reduction(+:tau0)
    
    do p=1, NAO
        tau0 = tau0 + ( &
            u(p)**2 * v(p)**2&
        )
    end do
    
    !$omp end do

    allocate(tau1(NAO))
    !$omp single
    tau1 = 0.0
    !$omp end single

    !$omp do schedule(static)
    do p=1, NAO
    
        tau1(p) = tau1(p) + ( &
            tau0 * one(p)&
        )
    
    end do
    !$omp end do

    !$omp single
    SzSz = 0.0
    !$omp end single

    !$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
    
            SzSz(p, q) = SzSz(p, q) + ( &
                one(q) * tau1(p)&
            )
    
        end do
    end do
    !$omp end do

    deallocate(tau1)

    allocate(tau2(NAO))
    !$omp single
    tau2 = 0.0
    !$omp end single

    !$omp do schedule(static)
    do p=1, NAO
    
        tau2(p) = tau2(p) + ( &
            t1(p) * v(p)&
        )
    
    end do
    !$omp end do

    !$omp single
    tau3 = 0.0
    !$omp end single

    !$omp do schedule(static) reduction(+:tau3)
    
    do p=1, NAO
        tau3 = tau3 + ( &
            u(p)**3 * tau2(p)&
        )
    end do
    
    !$omp end do

    deallocate(tau2)

    allocate(tau4(NAO))
    !$omp single
    tau4 = 0.0
    !$omp end single

    !$omp do schedule(static)
    do p=1, NAO
    
        tau4(p) = tau4(p) + ( &
            tau3 * one(p)&
        )
    
    end do
    !$omp end do

    !$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
    
            SzSz(p, q) = SzSz(p, q) + ( &
                one(p) * tau4(q)&
            )
    
        end do
    end do
    !$omp end do

    deallocate(tau4)

    allocate(tau5(NAO))
    !$omp single
    tau5 = 0.0
    !$omp end single

    !$omp do schedule(static)
    do p=1, NAO
    
        tau5(p) = tau5(p) + ( &
            v(p) * z1(p)&
        )
    
    end do
    !$omp end do

    !$omp single
    tau6 = 0.0
    !$omp end single

    !$omp do schedule(static) reduction(+:tau6)
    
    do p=1, NAO
        tau6 = tau6 + ( &
            u(p)**3 * tau5(p)&
        )
    end do
    
    !$omp end do

    allocate(tau7(NAO))
    !$omp single
    tau7 = 0.0
    !$omp end single

    !$omp do schedule(static)
    do p=1, NAO
    
        tau7(p) = tau7(p) + ( &
            tau6 * one(p)&
        )
    
    end do
    !$omp end do

    !$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
    
            SzSz(p, q) = SzSz(p, q) + ( &
                one(p) * tau7(q)&
            )
    
        end do
    end do
    !$omp end do

    deallocate(tau7)

    !$omp single
    tau29 = 0.0
    !$omp end single

    !$omp do schedule(static) reduction(+:tau29)
    
    do p=1, NAO
        tau29 = tau29 + ( &
            tau5(p) * u(p)&
        )
    end do
    
    !$omp end do

    allocate(tau30(NAO))
    !$omp single
    tau30 = 0.0
    !$omp end single

    !$omp do schedule(static)
    do p=1, NAO
    
        tau30(p) = tau30(p) + ( &
            tau29 * one(p)&
        )
    
    end do
    !$omp end do

    !$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
    
            SzSz(p, q) = SzSz(p, q) - ( &
                one(p) * tau30(q)&
            )
    
        end do
    end do
    !$omp end do

    allocate(tau31(NAO))
    !$omp single
    tau31 = 0.0
    !$omp end single

    !$omp do schedule(static)
    do p=1, NAO
    
        tau31(p) = tau31(p) + ( &
            tau5(p) * u(p)&
        )
    
    end do
    !$omp end do

    !$omp single
    tau32 = 0.0
    !$omp end single

    !$omp do schedule(static) reduction(+:tau32)
    
    do p=1, NAO
        tau32 = tau32 + ( &
            t1(p)**2 * tau31(p)&
        )
    end do
    
    !$omp end do

    deallocate(tau31)

    allocate(tau33(NAO))
    !$omp single
    tau33 = 0.0
    !$omp end single

    !$omp do schedule(static)
    do p=1, NAO
    
        tau33(p) = tau33(p) + ( &
            tau32 * one(p)&
        )
    
    end do
    !$omp end do

    !$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
    
            SzSz(p, q) = SzSz(p, q) + ( &
                one(p) * tau33(q)&
            )
    
        end do
    end do
    !$omp end do

    allocate(tau37(NAO))
    !$omp single
    tau37 = 0.0
    !$omp end single

    !$omp do schedule(static)
    do p=1, NAO
    
        tau37(p) = tau37(p) + ( &
            u(p)**3 * tau5(p)&
        )
    
    end do
    !$omp end do

    deallocate(tau5)

    !$omp single
    tau38 = 0.0
    !$omp end single

    !$omp do schedule(static) reduction(+:tau38)
    
    do p=1, NAO
        tau38 = tau38 + ( &
            t1(p)**2 * tau37(p)&
        )
    end do
    
    !$omp end do

    deallocate(tau37)

    allocate(tau39(NAO))
    !$omp single
    tau39 = 0.0
    !$omp end single

    !$omp do schedule(static)
    do p=1, NAO
    
        tau39(p) = tau39(p) + ( &
            tau38 * one(p)&
        )
    
    end do
    !$omp end do

    !$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
    
            SzSz(p, q) = SzSz(p, q) - ( &
                3 * one(p) * tau39(q)&
            )
    
        end do
    end do
    !$omp end do

    deallocate(tau39)

    allocate(tau8(NAO))
    !$omp single
    tau8 = 0.0
    !$omp end single

    !$omp do schedule(static)
    do p=1, NAO
    
        tau8(p) = tau8(p) + ( &
            t1(p) * z1(p)&
        )
    
    end do
    !$omp end do

    !$omp single
    tau9 = 0.0
    !$omp end single

    !$omp do schedule(static) reduction(+:tau9)
    
    do p=1, NAO
        tau9 = tau9 + ( &
            u(p)**4 * tau8(p)&
        )
    end do
    
    !$omp end do

    allocate(tau10(NAO))
    !$omp single
    tau10 = 0.0
    !$omp end single

    !$omp do schedule(static)
    do p=1, NAO
    
        tau10(p) = tau10(p) + ( &
            tau9 * one(p)&
        )
    
    end do
    !$omp end do

    !$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
    
            SzSz(p, q) = SzSz(p, q) + ( &
                one(p) * tau10(q)&
            )
    
        end do
    end do
    !$omp end do

    deallocate(tau10)

    !$omp single
    tau11 = 0.0
    !$omp end single

    !$omp do schedule(static) reduction(+:tau11)
    
    do p=1, NAO
        tau11 = tau11 + ( &
            v(p)**2 * tau8(p)&
        )
    end do
    
    !$omp end do

    allocate(tau12(NAO))
    !$omp single
    tau12 = 0.0
    !$omp end single

    !$omp do schedule(static)
    do p=1, NAO
    
        tau12(p) = tau12(p) + ( &
            tau11 * one(p)&
        )
    
    end do
    !$omp end do

    !$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
    
            SzSz(p, q) = SzSz(p, q) + ( &
                one(p) * tau12(q)&
            )
    
        end do
    end do
    !$omp end do

    !$omp single
    tau13 = 0.0
    !$omp end single

    !$omp do schedule(static) reduction(+:tau13)
    
    do p=1, NAO
        tau13 = tau13 + ( &
            v(p)**4 * tau8(p)&
        )
    end do
    
    !$omp end do

    allocate(tau14(NAO))
    !$omp single
    tau14 = 0.0
    !$omp end single

    !$omp do schedule(static)
    do p=1, NAO
    
        tau14(p) = tau14(p) + ( &
            tau13 * one(p)&
        )
    
    end do
    !$omp end do

    !$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
    
            SzSz(p, q) = SzSz(p, q) + ( &
                one(p) * tau14(q)&
            )
    
        end do
    end do
    !$omp end do

    deallocate(tau14)

    !$omp single
    tau18 = 0.0
    !$omp end single

    !$omp do schedule(static) reduction(+:tau18)
    
    do p=1, NAO
        tau18 = tau18 + ( &
            u(p)**2 * tau8(p)&
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
            tau18 * one(p)&
        )
    
    end do
    !$omp end do

    !$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
    
            SzSz(p, q) = SzSz(p, q) - ( &
                one(p) * tau19(q)&
            )
    
        end do
    end do
    !$omp end do

    allocate(tau34(NAO))
    !$omp single
    tau34 = 0.0
    !$omp end single

    !$omp do schedule(static)
    do p=1, NAO
    
        tau34(p) = tau34(p) + ( &
            v(p)**2 * tau8(p)&
        )
    
    end do
    !$omp end do

    deallocate(tau8)

    !$omp single
    tau35 = 0.0
    !$omp end single

    !$omp do schedule(static) reduction(+:tau35)
    
    do p=1, NAO
        tau35 = tau35 + ( &
            u(p)**2 * tau34(p)&
        )
    end do
    
    !$omp end do

    deallocate(tau34)

    allocate(tau36(NAO))
    !$omp single
    tau36 = 0.0
    !$omp end single

    !$omp do schedule(static)
    do p=1, NAO
    
        tau36(p) = tau36(p) + ( &
            tau35 * one(p)&
        )
    
    end do
    !$omp end do

    !$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
    
            SzSz(p, q) = SzSz(p, q) - ( &
                4 * one(p) * tau36(q)&
            )
    
        end do
    end do
    !$omp end do

    deallocate(tau36)

    allocate(tau15(NAO))
    !$omp single
    tau15 = 0.0
    !$omp end single

    !$omp do schedule(static)
    do p=1, NAO
    
        tau15(p) = tau15(p) + ( &
            u(p)**2 * v(p)**2&
        )
    
    end do
    !$omp end do

    !$omp single
    tau16 = 0.0
    !$omp end single

    !$omp do schedule(static) reduction(+:tau16)
    
    do p=1, NAO
        tau16 = tau16 + ( &
            t1(p)**2 * tau15(p)&
        )
    end do
    
    !$omp end do

    deallocate(tau15)

    allocate(tau17(NAO))
    !$omp single
    tau17 = 0.0
    !$omp end single

    !$omp do schedule(static)
    do p=1, NAO
    
        tau17(p) = tau17(p) + ( &
            tau16 * one(p)&
        )
    
    end do
    !$omp end do

    !$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
    
            SzSz(p, q) = SzSz(p, q) - ( &
                one(q) * tau17(p)&
            )
    
        end do
    end do
    !$omp end do

    deallocate(tau17)

    allocate(tau20(NAO))
    !$omp single
    tau20 = 0.0
    !$omp end single

    !$omp do schedule(static)
    do p=1, NAO
    
        tau20(p) = tau20(p) + ( &
            t1(p) * u(p)&
        )
    
    end do
    !$omp end do

    !$omp single
    tau21 = 0.0
    !$omp end single

    !$omp do schedule(static) reduction(+:tau21)
    
    do p=1, NAO
        tau21 = tau21 + ( &
            v(p)**3 * tau20(p)&
        )
    end do
    
    !$omp end do

    deallocate(tau20)

    allocate(tau22(NAO))
    !$omp single
    tau22 = 0.0
    !$omp end single

    !$omp do schedule(static)
    do p=1, NAO
    
        tau22(p) = tau22(p) + ( &
            tau21 * one(p)&
        )
    
    end do
    !$omp end do

    !$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
    
            SzSz(p, q) = SzSz(p, q) - ( &
                one(p) * tau22(q)&
            )
    
        end do
    end do
    !$omp end do

    deallocate(tau22)

    allocate(tau23(NAO))
    !$omp single
    tau23 = 0.0
    !$omp end single

    !$omp do schedule(static)
    do p=1, NAO
    
        tau23(p) = tau23(p) + ( &
            u(p) * z1(p)&
        )
    
    end do
    !$omp end do

    !$omp single
    tau24 = 0.0
    !$omp end single

    !$omp do schedule(static) reduction(+:tau24)
    
    do p=1, NAO
        tau24 = tau24 + ( &
            v(p)**3 * tau23(p)&
        )
    end do
    
    !$omp end do

    allocate(tau25(NAO))
    !$omp single
    tau25 = 0.0
    !$omp end single

    !$omp do schedule(static)
    do p=1, NAO
    
        tau25(p) = tau25(p) + ( &
            tau24 * one(p)&
        )
    
    end do
    !$omp end do

    !$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
    
            SzSz(p, q) = SzSz(p, q) - ( &
                one(p) * tau25(q)&
            )
    
        end do
    end do
    !$omp end do

    deallocate(tau25)

    allocate(tau44(NAO))
    !$omp single
    tau44 = 0.0
    !$omp end single

    !$omp do schedule(static)
    do p=1, NAO
    
        tau44(p) = tau44(p) + ( &
            v(p)**3 * tau23(p)&
        )
    
    end do
    !$omp end do

    deallocate(tau23)

    !$omp single
    tau45 = 0.0
    !$omp end single

    !$omp do schedule(static) reduction(+:tau45)
    
    do p=1, NAO
        tau45 = tau45 + ( &
            t1(p)**2 * tau44(p)&
        )
    end do
    
    !$omp end do

    deallocate(tau44)

    allocate(tau46(NAO))
    !$omp single
    tau46 = 0.0
    !$omp end single

    !$omp do schedule(static)
    do p=1, NAO
    
        tau46(p) = tau46(p) + ( &
            tau45 * one(p)&
        )
    
    end do
    !$omp end do

    !$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
    
            SzSz(p, q) = SzSz(p, q) + ( &
                3 * one(p) * tau46(q)&
            )
    
        end do
    end do
    !$omp end do

    deallocate(tau46)

    allocate(tau26(NAO))
    !$omp single
    tau26 = 0.0
    !$omp end single

    !$omp do schedule(static)
    do p=1, NAO
    
        tau26(p) = tau26(p) + ( &
            u(p) * v(p)&
        )
    
    end do
    !$omp end do

    !$omp single
    tau27 = 0.0
    !$omp end single

    !$omp do schedule(static) reduction(+:tau27)
    
    do p=1, NAO
        tau27 = tau27 + ( &
            t1(p) * tau26(p)&
        )
    end do
    
    !$omp end do

    allocate(tau28(NAO))
    !$omp single
    tau28 = 0.0
    !$omp end single

    !$omp do schedule(static)
    do p=1, NAO
    
        tau28(p) = tau28(p) + ( &
            tau27 * one(p)&
        )
    
    end do
    !$omp end do

    !$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
    
            SzSz(p, q) = SzSz(p, q) - ( &
                one(p) * tau28(q)&
            )
    
        end do
    end do
    !$omp end do

    !$omp single
    tau155 = 0.0
    !$omp end single

    !$omp single
    
    
    tau155 = tau155 + ( &
        tau27**2&
    )
    
    
    !$omp end single

    allocate(tau156(NAO))
    !$omp single
    tau156 = 0.0
    !$omp end single

    !$omp do schedule(static)
    do p=1, NAO
    
        tau156(p) = tau156(p) + ( &
            tau155 * one(p)&
        )
    
    end do
    !$omp end do

    !$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
    
            SzSz(p, q) = SzSz(p, q) + ( &
                one(p) * tau156(q)&
            )
    
        end do
    end do
    !$omp end do

    deallocate(tau156)

    !$omp single
    tau168 = 0.0
    !$omp end single

    !$omp single
    
    
    tau168 = tau168 + ( &
        tau11*tau27&
    )
    
    
    !$omp end single

    allocate(tau169(NAO))
    !$omp single
    tau169 = 0.0
    !$omp end single

    !$omp do schedule(static)
    do p=1, NAO
    
        tau169(p) = tau169(p) + ( &
            tau168 * one(p)&
        )
    
    end do
    !$omp end do

    !$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
    
            SzSz(p, q) = SzSz(p, q) - ( &
                2 * one(p) * tau169(q)&
            )
    
        end do
    end do
    !$omp end do

    deallocate(tau169)

    !$omp single
    tau174 = 0.0
    !$omp end single

    !$omp single
    
    
    tau174 = tau174 + ( &
        tau18*tau27&
    )
    
    
    !$omp end single

    allocate(tau175(NAO))
    !$omp single
    tau175 = 0.0
    !$omp end single

    !$omp do schedule(static)
    do p=1, NAO
    
        tau175(p) = tau175(p) + ( &
            tau174 * one(p)&
        )
    
    end do
    !$omp end do

    !$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
    
            SzSz(p, q) = SzSz(p, q) + ( &
                2 * one(p) * tau175(q)&
            )
    
        end do
    end do
    !$omp end do

    deallocate(tau175)

    !$omp single
    tau181 = 0.0
    !$omp end single

    !$omp single
    
    
    tau181 = tau181 + ( &
        tau27*tau29&
    )
    
    
    !$omp end single

    allocate(tau182(NAO))
    !$omp single
    tau182 = 0.0
    !$omp end single

    !$omp do schedule(static)
    do p=1, NAO
    
        tau182(p) = tau182(p) + ( &
            tau181 * one(p)&
        )
    
    end do
    !$omp end do

    !$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
    
            SzSz(p, q) = SzSz(p, q) + ( &
                2 * one(p) * tau182(q)&
            )
    
        end do
    end do
    !$omp end do

    deallocate(tau182)

    !$omp single
    tau195 = 0.0
    !$omp end single

    !$omp single
    
    
    tau195 = tau195 + ( &
        tau27*tau32&
    )
    
    
    !$omp end single

    allocate(tau196(NAO))
    !$omp single
    tau196 = 0.0
    !$omp end single

    !$omp do schedule(static)
    do p=1, NAO
    
        tau196(p) = tau196(p) + ( &
            tau195 * one(p)&
        )
    
    end do
    !$omp end do

    !$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
    
            SzSz(p, q) = SzSz(p, q) - ( &
                2 * one(p) * tau196(q)&
            )
    
        end do
    end do
    !$omp end do

    deallocate(tau196)

    allocate(tau53(NAO))
    !$omp single
    tau53 = 0.0
    !$omp end single

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            tau53(p) = tau53(p) + ( &
                tau26(q) * t2(p, q)&
            )
        end do
    end do
    !$omp end do

    allocate(tau54(NAO))
    !$omp single
    tau54 = 0.0
    !$omp end single

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            tau54(p) = tau54(p) + ( &
                tau53(q) * z2(q, p)&
            )
        end do
    end do
    !$omp end do

    allocate(tau55(NAO))
    !$omp single
    tau55 = 0.0
    !$omp end single

    !$omp do schedule(static)
    do p=1, NAO
    
        tau55(p) = tau55(p) + ( &
            t1(p) * tau54(p)&
        )
    
    end do
    !$omp end do

    !$omp single
    tau56 = 0.0
    !$omp end single

    !$omp do schedule(static) reduction(+:tau56)
    
    do p=1, NAO
        tau56 = tau56 + ( &
            v(p)**2 * tau55(p)&
        )
    end do
    
    !$omp end do

    allocate(tau57(NAO))
    !$omp single
    tau57 = 0.0
    !$omp end single

    !$omp do schedule(static)
    do p=1, NAO
    
        tau57(p) = tau57(p) + ( &
            tau56 * one(p)&
        )
    
    end do
    !$omp end do

    !$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
    
            SzSz(p, q) = SzSz(p, q) - ( &
                2 * one(p) * tau57(q)&
            )
    
        end do
    end do
    !$omp end do

    deallocate(tau57)

    !$omp single
    tau62 = 0.0
    !$omp end single

    !$omp do schedule(static) reduction(+:tau62)
    
    do p=1, NAO
        tau62 = tau62 + ( &
            u(p)**2 * tau55(p)&
        )
    end do
    
    !$omp end do

    deallocate(tau55)

    allocate(tau63(NAO))
    !$omp single
    tau63 = 0.0
    !$omp end single

    !$omp do schedule(static)
    do p=1, NAO
    
        tau63(p) = tau63(p) + ( &
            tau62 * one(p)&
        )
    
    end do
    !$omp end do

    !$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
    
            SzSz(p, q) = SzSz(p, q) + ( &
                2 * one(p) * tau63(q)&
            )
    
        end do
    end do
    !$omp end do

    deallocate(tau63)

    allocate(tau67(NAO))
    !$omp single
    tau67 = 0.0
    !$omp end single

    !$omp do schedule(static)
    do p=1, NAO
    
        tau67(p) = tau67(p) + ( &
            tau54(p) * v(p)&
        )
    
    end do
    !$omp end do

    !$omp single
    tau68 = 0.0
    !$omp end single

    !$omp do schedule(static) reduction(+:tau68)
    
    do p=1, NAO
        tau68 = tau68 + ( &
            tau67(p) * u(p)&
        )
    end do
    
    !$omp end do

    allocate(tau69(NAO))
    !$omp single
    tau69 = 0.0
    !$omp end single

    !$omp do schedule(static)
    do p=1, NAO
    
        tau69(p) = tau69(p) + ( &
            tau68 * one(p)&
        )
    
    end do
    !$omp end do

    !$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
    
            SzSz(p, q) = SzSz(p, q) + ( &
                2 * one(p) * tau69(q)&
            )
    
        end do
    end do
    !$omp end do

    deallocate(tau69)

    allocate(tau74(NAO))
    !$omp single
    tau74 = 0.0
    !$omp end single

    !$omp do schedule(static)
    do p=1, NAO
    
        tau74(p) = tau74(p) + ( &
            tau67(p) * u(p)&
        )
    
    end do
    !$omp end do

    deallocate(tau67)

    !$omp single
    tau75 = 0.0
    !$omp end single

    !$omp do schedule(static) reduction(+:tau75)
    
    do p=1, NAO
        tau75 = tau75 + ( &
            t1(p)**2 * tau74(p)&
        )
    end do
    
    !$omp end do

    deallocate(tau74)

    allocate(tau76(NAO))
    !$omp single
    tau76 = 0.0
    !$omp end single

    !$omp do schedule(static)
    do p=1, NAO
    
        tau76(p) = tau76(p) + ( &
            tau75 * one(p)&
        )
    
    end do
    !$omp end do

    !$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
    
            SzSz(p, q) = SzSz(p, q) - ( &
                2 * one(p) * tau76(q)&
            )
    
        end do
    end do
    !$omp end do

    deallocate(tau76)

    allocate(tau201(NAO))
    !$omp single
    tau201 = 0.0
    !$omp end single

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            tau201(p) = tau201(p) + ( &
                tau54(q) * t2(q, p)&
            )
        end do
    end do
    !$omp end do

    deallocate(tau54)

    allocate(tau202(NAO))
    !$omp single
    tau202 = 0.0
    !$omp end single

    !$omp do schedule(static)
    do p=1, NAO
    
        tau202(p) = tau202(p) + ( &
            tau201(p) * v(p)&
        )
    
    end do
    !$omp end do

    deallocate(tau201)

    !$omp single
    tau203 = 0.0
    !$omp end single

    !$omp do schedule(static) reduction(+:tau203)
    
    do p=1, NAO
        tau203 = tau203 + ( &
            tau202(p) * u(p)&
        )
    end do
    
    !$omp end do

    deallocate(tau202)

    allocate(tau204(NAO))
    !$omp single
    tau204 = 0.0
    !$omp end single

    !$omp do schedule(static)
    do p=1, NAO
    
        tau204(p) = tau204(p) + ( &
            tau203 * one(p)&
        )
    
    end do
    !$omp end do

    !$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
    
            SzSz(p, q) = SzSz(p, q) + ( &
                one(p) * tau204(q)&
            )
    
        end do
    end do
    !$omp end do

    deallocate(tau204)

    allocate(tau121(NAO))
    !$omp single
    tau121 = 0.0
    !$omp end single

    !$omp do schedule(static)
    do p=1, NAO
    
        tau121(p) = tau121(p) + ( &
            tau53(p) * v(p)&
        )
    
    end do
    !$omp end do

    !$omp single
    tau122 = 0.0
    !$omp end single

    !$omp do schedule(static) reduction(+:tau122)
    
    do p=1, NAO
        tau122 = tau122 + ( &
            tau121(p) * u(p)&
        )
    end do
    
    !$omp end do

    deallocate(tau121)

    allocate(tau123(NAO))
    !$omp single
    tau123 = 0.0
    !$omp end single

    !$omp do schedule(static)
    do p=1, NAO
    
        tau123(p) = tau123(p) + ( &
            tau122 * one(p)&
        )
    
    end do
    !$omp end do

    !$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
    
            SzSz(p, q) = SzSz(p, q) + ( &
                one(p) * tau123(q)&
            )
    
        end do
    end do
    !$omp end do

    deallocate(tau123)

    allocate(tau140(NAO))
    !$omp single
    tau140 = 0.0
    !$omp end single

    !$omp do schedule(static)
    do p=1, NAO
    
        tau140(p) = tau140(p) + ( &
            tau53(p) * z1(p)&
        )
    
    end do
    !$omp end do

    !$omp single
    tau141 = 0.0
    !$omp end single

    !$omp do schedule(static) reduction(+:tau141)
    
    do p=1, NAO
        tau141 = tau141 + ( &
            v(p)**2 * tau140(p)&
        )
    end do
    
    !$omp end do

    allocate(tau142(NAO))
    !$omp single
    tau142 = 0.0
    !$omp end single

    !$omp do schedule(static)
    do p=1, NAO
    
        tau142(p) = tau142(p) + ( &
            tau141 * one(p)&
        )
    
    end do
    !$omp end do

    !$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
    
            SzSz(p, q) = SzSz(p, q) - ( &
                2 * one(p) * tau142(q)&
            )
    
        end do
    end do
    !$omp end do

    deallocate(tau142)

    !$omp single
    tau145 = 0.0
    !$omp end single

    !$omp do schedule(static) reduction(+:tau145)
    
    do p=1, NAO
        tau145 = tau145 + ( &
            u(p)**2 * tau140(p)&
        )
    end do
    
    !$omp end do

    allocate(tau146(NAO))
    !$omp single
    tau146 = 0.0
    !$omp end single

    !$omp do schedule(static)
    do p=1, NAO
    
        tau146(p) = tau146(p) + ( &
            tau145 * one(p)&
        )
    
    end do
    !$omp end do

    !$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
    
            SzSz(p, q) = SzSz(p, q) + ( &
                2 * one(p) * tau146(q)&
            )
    
        end do
    end do
    !$omp end do

    deallocate(tau146)

    allocate(tau191(NAO))
    !$omp single
    tau191 = 0.0
    !$omp end single

    !$omp do schedule(static)
    do p=1, NAO
    
        tau191(p) = tau191(p) + ( &
            tau140(p) * v(p)&
        )
    
    end do
    !$omp end do

    deallocate(tau140)

    allocate(tau192(NAO))
    !$omp single
    tau192 = 0.0
    !$omp end single

    !$omp do schedule(static)
    do p=1, NAO
    
        tau192(p) = tau192(p) + ( &
            tau191(p) * u(p)&
        )
    
    end do
    !$omp end do

    deallocate(tau191)

    !$omp single
    tau193 = 0.0
    !$omp end single

    !$omp do schedule(static) reduction(+:tau193)
    
    do p=1, NAO
        tau193 = tau193 + ( &
            t1(p) * tau192(p)&
        )
    end do
    
    !$omp end do

    deallocate(tau192)

    allocate(tau194(NAO))
    !$omp single
    tau194 = 0.0
    !$omp end single

    !$omp do schedule(static)
    do p=1, NAO
    
        tau194(p) = tau194(p) + ( &
            tau193 * one(p)&
        )
    
    end do
    !$omp end do

    !$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
    
            SzSz(p, q) = SzSz(p, q) - ( &
                4 * one(p) * tau194(q)&
            )
    
        end do
    end do
    !$omp end do

    deallocate(tau194)

    allocate(tau124(NAO))
    !$omp single
    tau124 = 0.0
    !$omp end single

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            tau124(p) = tau124(p) + ( &
                tau26(q) * z2(p, q)&
            )
        end do
    end do
    !$omp end do

    allocate(tau125(NAO))
    !$omp single
    tau125 = 0.0
    !$omp end single

    !$omp do schedule(static)
    do p=1, NAO
    
        tau125(p) = tau125(p) + ( &
            tau124(p) * v(p)&
        )
    
    end do
    !$omp end do

    !$omp single
    tau126 = 0.0
    !$omp end single

    !$omp do schedule(static) reduction(+:tau126)
    
    do p=1, NAO
        tau126 = tau126 + ( &
            tau125(p) * u(p)&
        )
    end do
    
    !$omp end do

    allocate(tau127(NAO))
    !$omp single
    tau127 = 0.0
    !$omp end single

    !$omp do schedule(static)
    do p=1, NAO
    
        tau127(p) = tau127(p) + ( &
            tau126 * one(p)&
        )
    
    end do
    !$omp end do

    !$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
    
            SzSz(p, q) = SzSz(p, q) + ( &
                one(p) * tau127(q)&
            )
    
        end do
    end do
    !$omp end do

    deallocate(tau127)

    allocate(tau161(NAO))
    !$omp single
    tau161 = 0.0
    !$omp end single

    !$omp do schedule(static)
    do p=1, NAO
    
        tau161(p) = tau161(p) + ( &
            tau125(p) * u(p)&
        )
    
    end do
    !$omp end do

    deallocate(tau125)

    !$omp single
    tau162 = 0.0
    !$omp end single

    !$omp do schedule(static) reduction(+:tau162)
    
    do p=1, NAO
        tau162 = tau162 + ( &
            t1(p)**2 * tau161(p)&
        )
    end do
    
    !$omp end do

    deallocate(tau161)

    allocate(tau163(NAO))
    !$omp single
    tau163 = 0.0
    !$omp end single

    !$omp do schedule(static)
    do p=1, NAO
    
        tau163(p) = tau163(p) + ( &
            tau162 * one(p)&
        )
    
    end do
    !$omp end do

    !$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
    
            SzSz(p, q) = SzSz(p, q) - ( &
                2 * one(p) * tau163(q)&
            )
    
        end do
    end do
    !$omp end do

    deallocate(tau163)

    allocate(tau137(NAO))
    !$omp single
    tau137 = 0.0
    !$omp end single

    !$omp do schedule(static)
    do p=1, NAO
    
        tau137(p) = tau137(p) + ( &
            t1(p) * tau124(p)&
        )
    
    end do
    !$omp end do

    deallocate(tau124)

    !$omp single
    tau138 = 0.0
    !$omp end single

    !$omp do schedule(static) reduction(+:tau138)
    
    do p=1, NAO
        tau138 = tau138 + ( &
            v(p)**2 * tau137(p)&
        )
    end do
    
    !$omp end do

    allocate(tau139(NAO))
    !$omp single
    tau139 = 0.0
    !$omp end single

    !$omp do schedule(static)
    do p=1, NAO
    
        tau139(p) = tau139(p) + ( &
            tau138 * one(p)&
        )
    
    end do
    !$omp end do

    !$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
    
            SzSz(p, q) = SzSz(p, q) - ( &
                2 * one(p) * tau139(q)&
            )
    
        end do
    end do
    !$omp end do

    deallocate(tau139)

    !$omp single
    tau143 = 0.0
    !$omp end single

    !$omp do schedule(static) reduction(+:tau143)
    
    do p=1, NAO
        tau143 = tau143 + ( &
            u(p)**2 * tau137(p)&
        )
    end do
    
    !$omp end do

    deallocate(tau137)

    allocate(tau144(NAO))
    !$omp single
    tau144 = 0.0
    !$omp end single

    !$omp do schedule(static)
    do p=1, NAO
    
        tau144(p) = tau144(p) + ( &
            tau143 * one(p)&
        )
    
    end do
    !$omp end do

    !$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
    
            SzSz(p, q) = SzSz(p, q) + ( &
                2 * one(p) * tau144(q)&
            )
    
        end do
    end do
    !$omp end do

    deallocate(tau144)

    allocate(tau157(NAO))
    !$omp single
    tau157 = 0.0
    !$omp end single

    !$omp do schedule(static)
    do p=1, NAO
    
        tau157(p) = tau157(p) + ( &
            t1(p) * tau26(p)&
        )
    
    end do
    !$omp end do

    allocate(tau176(NAO, NAO))
    !$omp single
    tau176 = 0.0
    !$omp end single

    !$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
    
            tau176(p, q) = tau176(p, q) + ( &
                tau26(p) * z2(q, p)&
            )
    
        end do
    end do
    !$omp end do

    allocate(tau177(NAO))
    !$omp single
    tau177 = 0.0
    !$omp end single

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            tau177(p) = tau177(p) + ( &
                t2(p, q)**2 * tau176(q, p)&
            )
        end do
    end do
    !$omp end do

    deallocate(tau176)

    allocate(tau178(NAO))
    !$omp single
    tau178 = 0.0
    !$omp end single

    !$omp do schedule(static)
    do p=1, NAO
    
        tau178(p) = tau178(p) + ( &
            tau177(p) * v(p)&
        )
    
    end do
    !$omp end do

    deallocate(tau177)

    !$omp single
    tau179 = 0.0
    !$omp end single

    !$omp do schedule(static) reduction(+:tau179)
    
    do p=1, NAO
        tau179 = tau179 + ( &
            tau178(p) * u(p)&
        )
    end do
    
    !$omp end do

    deallocate(tau178)

    allocate(tau180(NAO))
    !$omp single
    tau180 = 0.0
    !$omp end single

    !$omp do schedule(static)
    do p=1, NAO
    
        tau180(p) = tau180(p) + ( &
            tau179 * one(p)&
        )
    
    end do
    !$omp end do

    !$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
    
            SzSz(p, q) = SzSz(p, q) + ( &
                2 * one(p) * tau180(q)&
            )
    
        end do
    end do
    !$omp end do

    deallocate(tau180)

    allocate(tau185(NAO))
    !$omp single
    tau185 = 0.0
    !$omp end single

    !$omp do schedule(static)
    do p=1, NAO
    
        tau185(p) = tau185(p) + ( &
            t1(p)**2 * tau26(p)&
        )
    
    end do
    !$omp end do

    deallocate(tau26)

    allocate(tau186(NAO))
    !$omp single
    tau186 = 0.0
    !$omp end single

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            tau186(p) = tau186(p) + ( &
                tau185(q) * z2(p, q)&
            )
        end do
    end do
    !$omp end do

    deallocate(tau185)

    allocate(tau187(NAO))
    !$omp single
    tau187 = 0.0
    !$omp end single

    !$omp do schedule(static)
    do p=1, NAO
    
        tau187(p) = tau187(p) + ( &
            tau186(p) * v(p)&
        )
    
    end do
    !$omp end do

    deallocate(tau186)

    allocate(tau188(NAO))
    !$omp single
    tau188 = 0.0
    !$omp end single

    !$omp do schedule(static)
    do p=1, NAO
    
        tau188(p) = tau188(p) + ( &
            tau187(p) * u(p)&
        )
    
    end do
    !$omp end do

    deallocate(tau187)

    !$omp single
    tau189 = 0.0
    !$omp end single

    !$omp do schedule(static) reduction(+:tau189)
    
    do p=1, NAO
        tau189 = tau189 + ( &
            t1(p)**2 * tau188(p)&
        )
    end do
    
    !$omp end do

    deallocate(tau188)

    allocate(tau190(NAO))
    !$omp single
    tau190 = 0.0
    !$omp end single

    !$omp do schedule(static)
    do p=1, NAO
    
        tau190(p) = tau190(p) + ( &
            tau189 * one(p)&
        )
    
    end do
    !$omp end do

    !$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
    
            SzSz(p, q) = SzSz(p, q) + ( &
                one(p) * tau190(q)&
            )
    
        end do
    end do
    !$omp end do

    deallocate(tau190)

    allocate(tau40(NAO))
    !$omp single
    tau40 = 0.0
    !$omp end single

    !$omp do schedule(static)
    do p=1, NAO
    
        tau40(p) = tau40(p) + ( &
            v(p)**2 * z1(p)&
        )
    
    end do
    !$omp end do

    allocate(tau41(NAO))
    !$omp single
    tau41 = 0.0
    !$omp end single

    !$omp do schedule(static)
    do p=1, NAO
    
        tau41(p) = tau41(p) + ( &
            u(p)**2 * tau40(p)&
        )
    
    end do
    !$omp end do

    deallocate(tau40)

    !$omp single
    tau42 = 0.0
    !$omp end single

    !$omp do schedule(static) reduction(+:tau42)
    
    do p=1, NAO
        tau42 = tau42 + ( &
            t1(p)**3 * tau41(p)&
        )
    end do
    
    !$omp end do

    deallocate(tau41)

    allocate(tau43(NAO))
    !$omp single
    tau43 = 0.0
    !$omp end single

    !$omp do schedule(static)
    do p=1, NAO
    
        tau43(p) = tau43(p) + ( &
            tau42 * one(p)&
        )
    
    end do
    !$omp end do

    !$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
    
            SzSz(p, q) = SzSz(p, q) + ( &
                2 * one(p) * tau43(q)&
            )
    
        end do
    end do
    !$omp end do

    deallocate(tau43)

    allocate(tau47(NAO))
    !$omp single
    tau47 = 0.0
    !$omp end single

    !$omp do schedule(static)
    do p=1, NAO
    
        tau47(p) = tau47(p) + ( &
            v(p)**2 * one(p)&
        )
    
    end do
    !$omp end do

    allocate(tau205(NAO, NAO))
    !$omp single
    tau205 = 0.0
    !$omp end single

    !$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
    
            tau205(p, q) = tau205(p, q) + ( &
                one(q) * tau47(p)&
            )
    
        end do
    end do
    !$omp end do

    allocate(tau215(NAO, NAO))
    !$omp single
    tau215 = 0.0
    !$omp end single

    !$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
    
            tau215(p, q) = tau215(p, q) - ( &
                tau205(p, q)&
            )
    
        end do
    end do
    !$omp end do

    deallocate(tau205)

    allocate(tau206(NAO, NAO))
    !$omp single
    tau206 = 0.0
    !$omp end single

    !$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
    
            tau206(p, q) = tau206(p, q) + ( &
                tau28(q) * tau47(p)&
            )
    
        end do
    end do
    !$omp end do

    deallocate(tau28)

    !$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
    
            tau215(p, q) = tau215(p, q) + ( &
                2 * tau206(p, q)&
            )
    
        end do
    end do
    !$omp end do

    deallocate(tau206)

    allocate(tau207(NAO, NAO))
    !$omp single
    tau207 = 0.0
    !$omp end single

    !$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
    
            tau207(p, q) = tau207(p, q) + ( &
                tau30(q) * tau47(p)&
            )
    
        end do
    end do
    !$omp end do

    deallocate(tau30)

    !$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
    
            tau215(p, q) = tau215(p, q) + ( &
                2 * tau207(p, q)&
            )
    
        end do
    end do
    !$omp end do

    deallocate(tau207)

    allocate(tau208(NAO, NAO))
    !$omp single
    tau208 = 0.0
    !$omp end single

    !$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
    
            tau208(p, q) = tau208(p, q) + ( &
                tau19(q) * tau47(p)&
            )
    
        end do
    end do
    !$omp end do

    deallocate(tau19)

    !$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
    
            tau215(p, q) = tau215(p, q) + ( &
                2 * tau208(p, q)&
            )
    
        end do
    end do
    !$omp end do

    deallocate(tau208)

    allocate(tau209(NAO, NAO))
    !$omp single
    tau209 = 0.0
    !$omp end single

    !$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
    
            tau209(p, q) = tau209(p, q) + ( &
                tau12(q) * tau47(p)&
            )
    
        end do
    end do
    !$omp end do

    deallocate(tau12)

    !$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
    
            tau215(p, q) = tau215(p, q) - ( &
                2 * tau209(p, q)&
            )
    
        end do
    end do
    !$omp end do

    deallocate(tau209)

    allocate(tau210(NAO, NAO))
    !$omp single
    tau210 = 0.0
    !$omp end single

    !$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
    
            tau210(p, q) = tau210(p, q) + ( &
                tau33(q) * tau47(p)&
            )
    
        end do
    end do
    !$omp end do

    deallocate(tau33)

    !$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
    
            tau215(p, q) = tau215(p, q) - ( &
                2 * tau210(p, q)&
            )
    
        end do
    end do
    !$omp end do

    deallocate(tau210)

    !$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
    
            SzSz(p, q) = SzSz(p, q) + ( &
                tau47(p) * tau47(q)&
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
            do r=1, NAO
                tau48(p, q) = tau48(p, q) + ( &
                    t2(r, p) * z2(q, r)&
                )
            end do
        end do
    end do
    !$omp end do

    allocate(tau49(NAO))
    !$omp single
    tau49 = 0.0
    !$omp end single

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            tau49(p) = tau49(p) + ( &
                t2(q, p) * tau48(p, q)&
            )
        end do
    end do
    !$omp end do

    deallocate(tau48)

    allocate(tau50(NAO))
    !$omp single
    tau50 = 0.0
    !$omp end single

    !$omp do schedule(static)
    do p=1, NAO
    
        tau50(p) = tau50(p) + ( &
            v(p)**2 * tau49(p)&
        )
    
    end do
    !$omp end do

    deallocate(tau49)

    !$omp single
    tau51 = 0.0
    !$omp end single

    !$omp do schedule(static) reduction(+:tau51)
    
    do p=1, NAO
        tau51 = tau51 + ( &
            u(p)**2 * tau50(p)&
        )
    end do
    
    !$omp end do

    deallocate(tau50)

    allocate(tau52(NAO))
    !$omp single
    tau52 = 0.0
    !$omp end single

    !$omp do schedule(static)
    do p=1, NAO
    
        tau52(p) = tau52(p) + ( &
            tau51 * one(p)&
        )
    
    end do
    !$omp end do

    !$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
    
            SzSz(p, q) = SzSz(p, q) - ( &
                one(p) * tau52(q)&
            )
    
        end do
    end do
    !$omp end do

    deallocate(tau52)

    allocate(tau58(NAO))
    !$omp single
    tau58 = 0.0
    !$omp end single

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            tau58(p) = tau58(p) + ( &
                t2(q, p) * z2(q, p)&
            )
        end do
    end do
    !$omp end do

    !$omp single
    tau59 = 0.0
    !$omp end single

    !$omp do schedule(static) reduction(+:tau59)
    
    do p=1, NAO
        tau59 = tau59 + ( &
            v(p)**2 * tau58(p)&
        )
    end do
    
    !$omp end do

    !$omp single
    tau60 = 0.0
    !$omp end single

    !$omp single
    
    
    tau60 = tau60 + ( &
        tau27*tau59&
    )
    
    
    !$omp end single

    allocate(tau61(NAO))
    !$omp single
    tau61 = 0.0
    !$omp end single

    !$omp do schedule(static)
    do p=1, NAO
    
        tau61(p) = tau61(p) + ( &
            tau60 * one(p)&
        )
    
    end do
    !$omp end do

    !$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
    
            SzSz(p, q) = SzSz(p, q) - ( &
                2 * one(p) * tau61(q)&
            )
    
        end do
    end do
    !$omp end do

    deallocate(tau61)

    allocate(tau89(NAO))
    !$omp single
    tau89 = 0.0
    !$omp end single

    !$omp do schedule(static)
    do p=1, NAO
    
        tau89(p) = tau89(p) + ( &
            tau59 * one(p)&
        )
    
    end do
    !$omp end do

    allocate(tau213(NAO, NAO))
    !$omp single
    tau213 = 0.0
    !$omp end single

    !$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
    
            tau213(p, q) = tau213(p, q) + ( &
                tau47(p) * tau89(q)&
            )
    
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
    
            tau215(p, q) = tau215(p, q) - ( &
                2 * tau213(p, q)&
            )
    
        end do
    end do
    !$omp end do

    deallocate(tau213)

    !$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
    
            SzSz(p, q) = SzSz(p, q) + ( &
                one(p) * tau89(q)&
            )
    
        end do
    end do
    !$omp end do

    deallocate(tau89)

    !$omp single
    tau64 = 0.0
    !$omp end single

    !$omp do schedule(static) reduction(+:tau64)
    
    do p=1, NAO
        tau64 = tau64 + ( &
            u(p)**2 * tau58(p)&
        )
    end do
    
    !$omp end do

    !$omp single
    tau65 = 0.0
    !$omp end single

    !$omp single
    
    
    tau65 = tau65 + ( &
        tau27*tau64&
    )
    
    
    !$omp end single

    allocate(tau66(NAO))
    !$omp single
    tau66 = 0.0
    !$omp end single

    !$omp do schedule(static)
    do p=1, NAO
    
        tau66(p) = tau66(p) + ( &
            tau65 * one(p)&
        )
    
    end do
    !$omp end do

    !$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
    
            SzSz(p, q) = SzSz(p, q) + ( &
                2 * one(p) * tau66(q)&
            )
    
        end do
    end do
    !$omp end do

    deallocate(tau66)

    allocate(tau92(NAO))
    !$omp single
    tau92 = 0.0
    !$omp end single

    !$omp do schedule(static)
    do p=1, NAO
    
        tau92(p) = tau92(p) + ( &
            tau64 * one(p)&
        )
    
    end do
    !$omp end do

    allocate(tau211(NAO, NAO))
    !$omp single
    tau211 = 0.0
    !$omp end single

    !$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
    
            tau211(p, q) = tau211(p, q) + ( &
                tau47(p) * tau92(q)&
            )
    
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
    
            tau215(p, q) = tau215(p, q) + ( &
                2 * tau211(p, q)&
            )
    
        end do
    end do
    !$omp end do

    deallocate(tau211)

    !$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
    
            SzSz(p, q) = SzSz(p, q) - ( &
                one(p) * tau92(q)&
            )
    
        end do
    end do
    !$omp end do

    deallocate(tau92)

    allocate(tau70(NAO))
    !$omp single
    tau70 = 0.0
    !$omp end single

    !$omp do schedule(static)
    do p=1, NAO
    
        tau70(p) = tau70(p) + ( &
            tau53(p) * tau58(p)&
        )
    
    end do
    !$omp end do

    deallocate(tau53)

    allocate(tau71(NAO))
    !$omp single
    tau71 = 0.0
    !$omp end single

    !$omp do schedule(static)
    do p=1, NAO
    
        tau71(p) = tau71(p) + ( &
            tau70(p) * v(p)&
        )
    
    end do
    !$omp end do

    deallocate(tau70)

    !$omp single
    tau72 = 0.0
    !$omp end single

    !$omp do schedule(static) reduction(+:tau72)
    
    do p=1, NAO
        tau72 = tau72 + ( &
            tau71(p) * u(p)&
        )
    end do
    
    !$omp end do

    deallocate(tau71)

    allocate(tau73(NAO))
    !$omp single
    tau73 = 0.0
    !$omp end single

    !$omp do schedule(static)
    do p=1, NAO
    
        tau73(p) = tau73(p) + ( &
            tau72 * one(p)&
        )
    
    end do
    !$omp end do

    !$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
    
            SzSz(p, q) = SzSz(p, q) - ( &
                4 * one(p) * tau73(q)&
            )
    
        end do
    end do
    !$omp end do

    deallocate(tau73)

    allocate(tau82(NAO))
    !$omp single
    tau82 = 0.0
    !$omp end single

    !$omp do schedule(static)
    do p=1, NAO
    
        tau82(p) = tau82(p) + ( &
            tau58(p) * v(p)&
        )
    
    end do
    !$omp end do

    allocate(tau83(NAO))
    !$omp single
    tau83 = 0.0
    !$omp end single

    !$omp do schedule(static)
    do p=1, NAO
    
        tau83(p) = tau83(p) + ( &
            tau82(p) * u(p)&
        )
    
    end do
    !$omp end do

    !$omp single
    tau84 = 0.0
    !$omp end single

    !$omp do schedule(static) reduction(+:tau84)
    
    do p=1, NAO
        tau84 = tau84 + ( &
            t1(p) * tau83(p)&
        )
    end do
    
    !$omp end do

    deallocate(tau83)

    !$omp single
    tau85 = 0.0
    !$omp end single

    !$omp single
    
    
    tau85 = tau85 + ( &
        tau27*tau84&
    )
    
    
    !$omp end single

    allocate(tau86(NAO))
    !$omp single
    tau86 = 0.0
    !$omp end single

    !$omp do schedule(static)
    do p=1, NAO
    
        tau86(p) = tau86(p) + ( &
            tau85 * one(p)&
        )
    
    end do
    !$omp end do

    !$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
    
            SzSz(p, q) = SzSz(p, q) - ( &
                4 * one(p) * tau86(q)&
            )
    
        end do
    end do
    !$omp end do

    deallocate(tau86)

    allocate(tau147(NAO))
    !$omp single
    tau147 = 0.0
    !$omp end single

    !$omp do schedule(static)
    do p=1, NAO
    
        tau147(p) = tau147(p) + ( &
            tau84 * one(p)&
        )
    
    end do
    !$omp end do

    allocate(tau214(NAO, NAO))
    !$omp single
    tau214 = 0.0
    !$omp end single

    !$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
    
            tau214(p, q) = tau214(p, q) + ( &
                tau147(q) * tau47(p)&
            )
    
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
    
            tau215(p, q) = tau215(p, q) - ( &
                4 * tau214(p, q)&
            )
    
        end do
    end do
    !$omp end do

    deallocate(tau214)

    !$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
    
            SzSz(p, q) = SzSz(p, q) + ( &
                2 * one(p) * tau147(q)&
            )
    
        end do
    end do
    !$omp end do

    deallocate(tau147)

    allocate(tau128(NAO))
    !$omp single
    tau128 = 0.0
    !$omp end single

    !$omp do schedule(static)
    do p=1, NAO
    
        tau128(p) = tau128(p) + ( &
            t1(p) * tau82(p)&
        )
    
    end do
    !$omp end do

    deallocate(tau82)

    !$omp single
    tau129 = 0.0
    !$omp end single

    !$omp do schedule(static) reduction(+:tau129)
    
    do p=1, NAO
        tau129 = tau129 + ( &
            u(p)**3 * tau128(p)&
        )
    end do
    
    !$omp end do

    deallocate(tau128)

    allocate(tau130(NAO))
    !$omp single
    tau130 = 0.0
    !$omp end single

    !$omp do schedule(static)
    do p=1, NAO
    
        tau130(p) = tau130(p) + ( &
            tau129 * one(p)&
        )
    
    end do
    !$omp end do

    !$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
    
            SzSz(p, q) = SzSz(p, q) - ( &
                6 * one(p) * tau130(q)&
            )
    
        end do
    end do
    !$omp end do

    deallocate(tau130)

    !$omp single
    tau87 = 0.0
    !$omp end single

    !$omp do schedule(static) reduction(+:tau87)
    
    do p=1, NAO
        tau87 = tau87 + ( &
            u(p)**4 * tau58(p)&
        )
    end do
    
    !$omp end do

    allocate(tau88(NAO))
    !$omp single
    tau88 = 0.0
    !$omp end single

    !$omp do schedule(static)
    do p=1, NAO
    
        tau88(p) = tau88(p) + ( &
            tau87 * one(p)&
        )
    
    end do
    !$omp end do

    !$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
    
            SzSz(p, q) = SzSz(p, q) + ( &
                one(p) * tau88(q)&
            )
    
        end do
    end do
    !$omp end do

    deallocate(tau88)

    !$omp single
    tau90 = 0.0
    !$omp end single

    !$omp do schedule(static) reduction(+:tau90)
    
    do p=1, NAO
        tau90 = tau90 + ( &
            v(p)**4 * tau58(p)&
        )
    end do
    
    !$omp end do

    allocate(tau91(NAO))
    !$omp single
    tau91 = 0.0
    !$omp end single

    !$omp do schedule(static)
    do p=1, NAO
    
        tau91(p) = tau91(p) + ( &
            tau90 * one(p)&
        )
    
    end do
    !$omp end do

    !$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
    
            SzSz(p, q) = SzSz(p, q) + ( &
                one(p) * tau91(q)&
            )
    
        end do
    end do
    !$omp end do

    deallocate(tau91)

    allocate(tau106(NAO))
    !$omp single
    tau106 = 0.0
    !$omp end single

    !$omp do schedule(static)
    do p=1, NAO
    
        tau106(p) = tau106(p) + ( &
            v(p)**2 * tau58(p)&
        )
    
    end do
    !$omp end do

    !$omp single
    tau107 = 0.0
    !$omp end single

    !$omp do schedule(static) reduction(+:tau107)
    
    do p=1, NAO
        tau107 = tau107 + ( &
            u(p)**2 * tau106(p)&
        )
    end do
    
    !$omp end do

    allocate(tau108(NAO))
    !$omp single
    tau108 = 0.0
    !$omp end single

    !$omp do schedule(static)
    do p=1, NAO
    
        tau108(p) = tau108(p) + ( &
            tau107 * one(p)&
        )
    
    end do
    !$omp end do

    !$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
    
            SzSz(p, q) = SzSz(p, q) - ( &
                4 * one(p) * tau108(q)&
            )
    
        end do
    end do
    !$omp end do

    deallocate(tau108)

    allocate(tau148(NAO))
    !$omp single
    tau148 = 0.0
    !$omp end single

    !$omp do schedule(static)
    do p=1, NAO
    
        tau148(p) = tau148(p) + ( &
            u(p)**2 * tau106(p)&
        )
    
    end do
    !$omp end do

    deallocate(tau106)

    !$omp single
    tau149 = 0.0
    !$omp end single

    !$omp do schedule(static) reduction(+:tau149)
    
    do p=1, NAO
        tau149 = tau149 + ( &
            t1(p)**2 * tau148(p)&
        )
    end do
    
    !$omp end do

    deallocate(tau148)

    allocate(tau150(NAO))
    !$omp single
    tau150 = 0.0
    !$omp end single

    !$omp do schedule(static)
    do p=1, NAO
    
        tau150(p) = tau150(p) + ( &
            tau149 * one(p)&
        )
    
    end do
    !$omp end do

    !$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
    
            SzSz(p, q) = SzSz(p, q) + ( &
                6 * one(p) * tau150(q)&
            )
    
        end do
    end do
    !$omp end do

    deallocate(tau150)

    allocate(tau151(NAO))
    !$omp single
    tau151 = 0.0
    !$omp end single

    !$omp do schedule(static)
    do p=1, NAO
    
        tau151(p) = tau151(p) + ( &
            tau58(p) * u(p)&
        )
    
    end do
    !$omp end do

    deallocate(tau58)

    allocate(tau152(NAO))
    !$omp single
    tau152 = 0.0
    !$omp end single

    !$omp do schedule(static)
    do p=1, NAO
    
        tau152(p) = tau152(p) + ( &
            t1(p) * tau151(p)&
        )
    
    end do
    !$omp end do

    deallocate(tau151)

    !$omp single
    tau153 = 0.0
    !$omp end single

    !$omp do schedule(static) reduction(+:tau153)
    
    do p=1, NAO
        tau153 = tau153 + ( &
            v(p)**3 * tau152(p)&
        )
    end do
    
    !$omp end do

    deallocate(tau152)

    allocate(tau154(NAO))
    !$omp single
    tau154 = 0.0
    !$omp end single

    !$omp do schedule(static)
    do p=1, NAO
    
        tau154(p) = tau154(p) + ( &
            tau153 * one(p)&
        )
    
    end do
    !$omp end do

    !$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
    
            SzSz(p, q) = SzSz(p, q) + ( &
                6 * one(p) * tau154(q)&
            )
    
        end do
    end do
    !$omp end do

    deallocate(tau154)

    allocate(tau77(NAO))
    !$omp single
    tau77 = 0.0
    !$omp end single

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            tau77(p) = tau77(p) + ( &
                z1(q) * t2(q, p)&
            )
        end do
    end do
    !$omp end do

    allocate(tau78(NAO))
    !$omp single
    tau78 = 0.0
    !$omp end single

    !$omp do schedule(static)
    do p=1, NAO
    
        tau78(p) = tau78(p) + ( &
            tau77(p) * v(p)&
        )
    
    end do
    !$omp end do

    !$omp single
    tau79 = 0.0
    !$omp end single

    !$omp do schedule(static) reduction(+:tau79)
    
    do p=1, NAO
        tau79 = tau79 + ( &
            tau78(p) * u(p)&
        )
    end do
    
    !$omp end do

    !$omp single
    tau80 = 0.0
    !$omp end single

    !$omp single
    
    
    tau80 = tau80 + ( &
        tau27*tau79&
    )
    
    
    !$omp end single

    allocate(tau81(NAO))
    !$omp single
    tau81 = 0.0
    !$omp end single

    !$omp do schedule(static)
    do p=1, NAO
    
        tau81(p) = tau81(p) + ( &
            tau80 * one(p)&
        )
    
    end do
    !$omp end do

    !$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
    
            SzSz(p, q) = SzSz(p, q) + ( &
                2 * one(p) * tau81(q)&
            )
    
        end do
    end do
    !$omp end do

    deallocate(tau81)

    allocate(tau105(NAO))
    !$omp single
    tau105 = 0.0
    !$omp end single

    !$omp do schedule(static)
    do p=1, NAO
    
        tau105(p) = tau105(p) + ( &
            tau79 * one(p)&
        )
    
    end do
    !$omp end do

    allocate(tau212(NAO, NAO))
    !$omp single
    tau212 = 0.0
    !$omp end single

    !$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
    
            tau212(p, q) = tau212(p, q) + ( &
                tau105(q) * tau47(p)&
            )
    
        end do
    end do
    !$omp end do

    deallocate(tau47)

    !$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
    
            tau215(p, q) = tau215(p, q) + ( &
                2 * tau212(p, q)&
            )
    
        end do
    end do
    !$omp end do

    deallocate(tau212)

    !$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
    
            SzSz(p, q) = SzSz(p, q) + ( &
                tau215(p, q) / 2&
            )
    
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
    
            SzSz(p, q) = SzSz(p, q) + ( &
                tau215(q, p) / 2&
            )
    
        end do
    end do
    !$omp end do

    deallocate(tau215)

    !$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
    
            SzSz(p, q) = SzSz(p, q) - ( &
                one(p) * tau105(q)&
            )
    
        end do
    end do
    !$omp end do

    deallocate(tau105)

    !$omp single
    tau97 = 0.0
    !$omp end single

    !$omp do schedule(static) reduction(+:tau97)
    
    do p=1, NAO
        tau97 = tau97 + ( &
            u(p)**3 * tau78(p)&
        )
    end do
    
    !$omp end do

    deallocate(tau78)

    allocate(tau98(NAO))
    !$omp single
    tau98 = 0.0
    !$omp end single

    !$omp do schedule(static)
    do p=1, NAO
    
        tau98(p) = tau98(p) + ( &
            tau97 * one(p)&
        )
    
    end do
    !$omp end do

    !$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
    
            SzSz(p, q) = SzSz(p, q) + ( &
                one(p) * tau98(q)&
            )
    
        end do
    end do
    !$omp end do

    deallocate(tau98)

    allocate(tau102(NAO))
    !$omp single
    tau102 = 0.0
    !$omp end single

    !$omp do schedule(static)
    do p=1, NAO
    
        tau102(p) = tau102(p) + ( &
            tau77(p) * u(p)&
        )
    
    end do
    !$omp end do

    !$omp single
    tau103 = 0.0
    !$omp end single

    !$omp do schedule(static) reduction(+:tau103)
    
    do p=1, NAO
        tau103 = tau103 + ( &
            v(p)**3 * tau102(p)&
        )
    end do
    
    !$omp end do

    deallocate(tau102)

    allocate(tau104(NAO))
    !$omp single
    tau104 = 0.0
    !$omp end single

    !$omp do schedule(static)
    do p=1, NAO
    
        tau104(p) = tau104(p) + ( &
            tau103 * one(p)&
        )
    
    end do
    !$omp end do

    !$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
    
            SzSz(p, q) = SzSz(p, q) - ( &
                one(p) * tau104(q)&
            )
    
        end do
    end do
    !$omp end do

    deallocate(tau104)

    allocate(tau131(NAO))
    !$omp single
    tau131 = 0.0
    !$omp end single

    !$omp do schedule(static)
    do p=1, NAO
    
        tau131(p) = tau131(p) + ( &
            t1(p) * tau77(p)&
        )
    
    end do
    !$omp end do

    deallocate(tau77)

    allocate(tau132(NAO))
    !$omp single
    tau132 = 0.0
    !$omp end single

    !$omp do schedule(static)
    do p=1, NAO
    
        tau132(p) = tau132(p) + ( &
            v(p)**2 * tau131(p)&
        )
    
    end do
    !$omp end do

    deallocate(tau131)

    !$omp single
    tau133 = 0.0
    !$omp end single

    !$omp do schedule(static) reduction(+:tau133)
    
    do p=1, NAO
        tau133 = tau133 + ( &
            u(p)**2 * tau132(p)&
        )
    end do
    
    !$omp end do

    deallocate(tau132)

    allocate(tau134(NAO))
    !$omp single
    tau134 = 0.0
    !$omp end single

    !$omp do schedule(static)
    do p=1, NAO
    
        tau134(p) = tau134(p) + ( &
            tau133 * one(p)&
        )
    
    end do
    !$omp end do

    !$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
    
            SzSz(p, q) = SzSz(p, q) - ( &
                2 * one(p) * tau134(q)&
            )
    
        end do
    end do
    !$omp end do

    deallocate(tau134)

    allocate(tau93(NAO, NAO))
    !$omp single
    tau93 = 0.0
    !$omp end single

    !$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
    
            tau93(p, q) = tau93(p, q) + ( &
                t2(p, q) * z2(p, q)&
            )
    
        end do
    end do
    !$omp end do

    allocate(tau94(NAO))
    !$omp single
    tau94 = 0.0
    !$omp end single

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            tau94(p) = tau94(p) + ( &
                u(q)**2 * tau93(p, q)&
            )
        end do
    end do
    !$omp end do

    !$omp single
    tau95 = 0.0
    !$omp end single

    !$omp do schedule(static) reduction(+:tau95)
    
    do p=1, NAO
        tau95 = tau95 + ( &
            u(p)**2 * tau94(p)&
        )
    end do
    
    !$omp end do

    deallocate(tau94)

    allocate(tau96(NAO))
    !$omp single
    tau96 = 0.0
    !$omp end single

    !$omp do schedule(static)
    do p=1, NAO
    
        tau96(p) = tau96(p) + ( &
            tau95 * one(p)&
        )
    
    end do
    !$omp end do

    !$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
    
            SzSz(p, q) = SzSz(p, q) + ( &
                one(p) * tau96(q)&
            )
    
        end do
    end do
    !$omp end do

    deallocate(tau96)

    allocate(tau99(NAO))
    !$omp single
    tau99 = 0.0
    !$omp end single

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            tau99(p) = tau99(p) + ( &
                v(q)**2 * tau93(p, q)&
            )
        end do
    end do
    !$omp end do

    !$omp single
    tau100 = 0.0
    !$omp end single

    !$omp do schedule(static) reduction(+:tau100)
    
    do p=1, NAO
        tau100 = tau100 + ( &
            v(p)**2 * tau99(p)&
        )
    end do
    
    !$omp end do

    allocate(tau101(NAO))
    !$omp single
    tau101 = 0.0
    !$omp end single

    !$omp do schedule(static)
    do p=1, NAO
    
        tau101(p) = tau101(p) + ( &
            tau100 * one(p)&
        )
    
    end do
    !$omp end do

    !$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
    
            SzSz(p, q) = SzSz(p, q) + ( &
                one(p) * tau101(q)&
            )
    
        end do
    end do
    !$omp end do

    deallocate(tau101)

    !$omp single
    tau109 = 0.0
    !$omp end single

    !$omp do schedule(static) reduction(+:tau109)
    
    do p=1, NAO
        tau109 = tau109 + ( &
            u(p)**2 * tau99(p)&
        )
    end do
    
    !$omp end do

    deallocate(tau99)

    allocate(tau110(NAO))
    !$omp single
    tau110 = 0.0
    !$omp end single

    !$omp do schedule(static)
    do p=1, NAO
    
        tau110(p) = tau110(p) + ( &
            tau109 * one(p)&
        )
    
    end do
    !$omp end do

    !$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
    
            SzSz(p, q) = SzSz(p, q) - ( &
                2 * one(p) * tau110(q)&
            )
    
        end do
    end do
    !$omp end do

    deallocate(tau110)

    allocate(tau158(NAO))
    !$omp single
    tau158 = 0.0
    !$omp end single

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            tau158(p) = tau158(p) + ( &
                tau157(q) * tau93(q, p)&
            )
        end do
    end do
    !$omp end do

    deallocate(tau93)

    deallocate(tau157)

    !$omp single
    tau159 = 0.0
    !$omp end single

    !$omp do schedule(static) reduction(+:tau159)
    
    do p=1, NAO
        tau159 = tau159 + ( &
            u(p)**2 * tau158(p)&
        )
    end do
    
    !$omp end do

    allocate(tau160(NAO))
    !$omp single
    tau160 = 0.0
    !$omp end single

    !$omp do schedule(static)
    do p=1, NAO
    
        tau160(p) = tau160(p) + ( &
            tau159 * one(p)&
        )
    
    end do
    !$omp end do

    !$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
    
            SzSz(p, q) = SzSz(p, q) - ( &
                4 * one(p) * tau160(q)&
            )
    
        end do
    end do
    !$omp end do

    deallocate(tau160)

    !$omp single
    tau183 = 0.0
    !$omp end single

    !$omp do schedule(static) reduction(+:tau183)
    
    do p=1, NAO
        tau183 = tau183 + ( &
            v(p)**2 * tau158(p)&
        )
    end do
    
    !$omp end do

    allocate(tau184(NAO))
    !$omp single
    tau184 = 0.0
    !$omp end single

    !$omp do schedule(static)
    do p=1, NAO
    
        tau184(p) = tau184(p) + ( &
            tau183 * one(p)&
        )
    
    end do
    !$omp end do

    !$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
    
            SzSz(p, q) = SzSz(p, q) + ( &
                4 * one(p) * tau184(q)&
            )
    
        end do
    end do
    !$omp end do

    deallocate(tau184)

    allocate(tau197(NAO))
    !$omp single
    tau197 = 0.0
    !$omp end single

    !$omp do schedule(static)
    do p=1, NAO
    
        tau197(p) = tau197(p) + ( &
            tau158(p) * v(p)&
        )
    
    end do
    !$omp end do

    deallocate(tau158)

    allocate(tau198(NAO))
    !$omp single
    tau198 = 0.0
    !$omp end single

    !$omp do schedule(static)
    do p=1, NAO
    
        tau198(p) = tau198(p) + ( &
            tau197(p) * u(p)&
        )
    
    end do
    !$omp end do

    deallocate(tau197)

    !$omp single
    tau199 = 0.0
    !$omp end single

    !$omp do schedule(static) reduction(+:tau199)
    
    do p=1, NAO
        tau199 = tau199 + ( &
            t1(p) * tau198(p)&
        )
    end do
    
    !$omp end do

    deallocate(tau198)

    allocate(tau200(NAO))
    !$omp single
    tau200 = 0.0
    !$omp end single

    !$omp do schedule(static)
    do p=1, NAO
    
        tau200(p) = tau200(p) + ( &
            tau199 * one(p)&
        )
    
    end do
    !$omp end do

    !$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
    
            SzSz(p, q) = SzSz(p, q) + ( &
                4 * one(p) * tau200(q)&
            )
    
        end do
    end do
    !$omp end do

    deallocate(tau200)

    allocate(tau111(NAO))
    !$omp single
    tau111 = 0.0
    !$omp end single

    !$omp do schedule(static)
    do p=1, NAO
    
        tau111(p) = tau111(p) + ( &
            u(p)**2 * t1(p)&
        )
    
    end do
    !$omp end do

    allocate(tau112(NAO))
    !$omp single
    tau112 = 0.0
    !$omp end single

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            tau112(p) = tau112(p) + ( &
                tau111(q) * z2(p, q)&
            )
        end do
    end do
    !$omp end do

    deallocate(tau111)

    allocate(tau113(NAO))
    !$omp single
    tau113 = 0.0
    !$omp end single

    !$omp do schedule(static)
    do p=1, NAO
    
        tau113(p) = tau113(p) + ( &
            t1(p) * tau112(p)&
        )
    
    end do
    !$omp end do

    !$omp single
    tau114 = 0.0
    !$omp end single

    !$omp do schedule(static) reduction(+:tau114)
    
    do p=1, NAO
        tau114 = tau114 + ( &
            u(p)**2 * tau113(p)&
        )
    end do
    
    !$omp end do

    deallocate(tau113)

    allocate(tau115(NAO))
    !$omp single
    tau115 = 0.0
    !$omp end single

    !$omp do schedule(static)
    do p=1, NAO
    
        tau115(p) = tau115(p) + ( &
            tau114 * one(p)&
        )
    
    end do
    !$omp end do

    !$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
    
            SzSz(p, q) = SzSz(p, q) + ( &
                one(p) * tau115(q)&
            )
    
        end do
    end do
    !$omp end do

    deallocate(tau115)

    allocate(tau164(NAO))
    !$omp single
    tau164 = 0.0
    !$omp end single

    !$omp do schedule(static)
    do p=1, NAO
    
        tau164(p) = tau164(p) + ( &
            tau112(p) * v(p)&
        )
    
    end do
    !$omp end do

    deallocate(tau112)

    allocate(tau165(NAO))
    !$omp single
    tau165 = 0.0
    !$omp end single

    !$omp do schedule(static)
    do p=1, NAO
    
        tau165(p) = tau165(p) + ( &
            tau164(p) * u(p)&
        )
    
    end do
    !$omp end do

    deallocate(tau164)

    !$omp single
    tau166 = 0.0
    !$omp end single

    !$omp do schedule(static) reduction(+:tau166)
    
    do p=1, NAO
        tau166 = tau166 + ( &
            t1(p)**2 * tau165(p)&
        )
    end do
    
    !$omp end do

    deallocate(tau165)

    allocate(tau167(NAO))
    !$omp single
    tau167 = 0.0
    !$omp end single

    !$omp do schedule(static)
    do p=1, NAO
    
        tau167(p) = tau167(p) + ( &
            tau166 * one(p)&
        )
    
    end do
    !$omp end do

    !$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
    
            SzSz(p, q) = SzSz(p, q) - ( &
                2 * one(p) * tau167(q)&
            )
    
        end do
    end do
    !$omp end do

    deallocate(tau167)

    allocate(tau116(NAO))
    !$omp single
    tau116 = 0.0
    !$omp end single

    !$omp do schedule(static)
    do p=1, NAO
    
        tau116(p) = tau116(p) + ( &
            v(p)**2 * t1(p)&
        )
    
    end do
    !$omp end do

    allocate(tau117(NAO))
    !$omp single
    tau117 = 0.0
    !$omp end single

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            tau117(p) = tau117(p) + ( &
                tau116(q) * z2(p, q)&
            )
        end do
    end do
    !$omp end do

    deallocate(tau116)

    allocate(tau118(NAO))
    !$omp single
    tau118 = 0.0
    !$omp end single

    !$omp do schedule(static)
    do p=1, NAO
    
        tau118(p) = tau118(p) + ( &
            t1(p) * tau117(p)&
        )
    
    end do
    !$omp end do

    !$omp single
    tau119 = 0.0
    !$omp end single

    !$omp do schedule(static) reduction(+:tau119)
    
    do p=1, NAO
        tau119 = tau119 + ( &
            v(p)**2 * tau118(p)&
        )
    end do
    
    !$omp end do

    allocate(tau120(NAO))
    !$omp single
    tau120 = 0.0
    !$omp end single

    !$omp do schedule(static)
    do p=1, NAO
    
        tau120(p) = tau120(p) + ( &
            tau119 * one(p)&
        )
    
    end do
    !$omp end do

    !$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
    
            SzSz(p, q) = SzSz(p, q) + ( &
                one(p) * tau120(q)&
            )
    
        end do
    end do
    !$omp end do

    deallocate(tau120)

    !$omp single
    tau135 = 0.0
    !$omp end single

    !$omp do schedule(static) reduction(+:tau135)
    
    do p=1, NAO
        tau135 = tau135 + ( &
            u(p)**2 * tau118(p)&
        )
    end do
    
    !$omp end do

    deallocate(tau118)

    allocate(tau136(NAO))
    !$omp single
    tau136 = 0.0
    !$omp end single

    !$omp do schedule(static)
    do p=1, NAO
    
        tau136(p) = tau136(p) + ( &
            tau135 * one(p)&
        )
    
    end do
    !$omp end do

    !$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
    
            SzSz(p, q) = SzSz(p, q) - ( &
                2 * one(p) * tau136(q)&
            )
    
        end do
    end do
    !$omp end do

    deallocate(tau136)

    allocate(tau170(NAO))
    !$omp single
    tau170 = 0.0
    !$omp end single

    !$omp do schedule(static)
    do p=1, NAO
    
        tau170(p) = tau170(p) + ( &
            tau117(p) * v(p)&
        )
    
    end do
    !$omp end do

    deallocate(tau117)

    allocate(tau171(NAO))
    !$omp single
    tau171 = 0.0
    !$omp end single

    !$omp do schedule(static)
    do p=1, NAO
    
        tau171(p) = tau171(p) + ( &
            tau170(p) * u(p)&
        )
    
    end do
    !$omp end do

    deallocate(tau170)

    !$omp single
    tau172 = 0.0
    !$omp end single

    !$omp do schedule(static) reduction(+:tau172)
    
    do p=1, NAO
        tau172 = tau172 + ( &
            t1(p)**2 * tau171(p)&
        )
    end do
    
    !$omp end do

    deallocate(tau171)

    allocate(tau173(NAO))
    !$omp single
    tau173 = 0.0
    !$omp end single

    !$omp do schedule(static)
    do p=1, NAO
    
        tau173(p) = tau173(p) + ( &
            tau172 * one(p)&
        )
    
    end do
    !$omp end do

    !$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
    
            SzSz(p, q) = SzSz(p, q) + ( &
                2 * one(p) * tau173(q)&
            )
    
        end do
    end do
    !$omp end do

    deallocate(tau173)

    !$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
    
            SzSz(p, q) = SzSz(p, q) + ( &
                one(p) * one(q) / 4&
            )
    
        end do
    end do
    !$omp end do

    !$omp end parallel

    do p = 1, NAO
    do q = p, NAO
       tmp = 0.5_pr * (SzSz(p,q) + (SzSz(q,p)))
       !tmp = 0.5_pr * (W002(p,q) + (conjg(W002(q,p)))
       SzSz(p,q) = tmp
       SzSz(q,p) = (tmp)
        
    end do
    end do

    end Subroutine CCSD_SzSz
    End Module CCSzSz
