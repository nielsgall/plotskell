{-# LANGUAGE BangPatterns #-}

module GhettoLM where

import Data.Vector.Storable
import qualified Data.Vector.Storable as V
import Foreign
import Foreign.C.Types
import Graphics.Rendering.OpenGL


data Vec3 = Vec3 {-# UNPACK #-} !GLfloat
                 {-# UNPACK #-} !GLfloat
                 {-# UNPACK #-} !GLfloat

data Vec4 = Vec4 {-# UNPACK #-} !GLfloat
                 {-# UNPACK #-} !GLfloat
                 {-# UNPACK #-} !GLfloat
                 {-# UNPACK #-} !GLfloat

data Mat4 = Mat4 {-# UNPACK #-} !Vec4
                 {-# UNPACK #-} !Vec4
                 {-# UNPACK #-} !Vec4
                 {-# UNPACK #-} !Vec4


instance Storable Vec3 where
  sizeOf _ = sizeOf (undefined :: GLfloat) * 3
  alignment _ = alignment (undefined :: GLfloat)
 
  {-# INLINE peek #-}
  peek p = do
             a <- peekElemOff q 0
             b <- peekElemOff q 1
             c <- peekElemOff q 2
             return (Vec3 a b c)
    where
      q = castPtr p
  {-# INLINE poke #-}
  poke p (Vec3 a b c) = do
             pokeElemOff q 0 a
             pokeElemOff q 1 b
             pokeElemOff q 2 c
    where
      q = castPtr p

instance Storable Vec4 where
  sizeOf _ = sizeOf (undefined :: GLfloat) * 4
  alignment _ = alignment (undefined :: GLfloat)
 
  {-# INLINE peek #-}
  peek p = do
             a <- peekElemOff q 0
             b <- peekElemOff q 1
             c <- peekElemOff q 2
             d <- peekElemOff q 3
             return (Vec4 a b c d)
    where
      q = castPtr p
  {-# INLINE poke #-}
  poke p (Vec4 a b c d) = do
             pokeElemOff q 0 a
             pokeElemOff q 1 b
             pokeElemOff q 2 c
             pokeElemOff q 3 d
    where
      q = castPtr p


printVec3 v@(Vec3 a b c) = do
                        print "start vector3"
                        print a
                        print b
                        print c
                        print $ sizeOf v
                        print "end vector3"

printVec4 v@(Vec4 a b c d) = do
                        print "start vector4"
                        print a
                        print b
                        print c
                        print d
                        print $ sizeOf v
                        print "end vector4"

printVec4d :: Vec4 -> IO ()
printVec4d v@(Vec4 a b c d) = do
        putStrLn ((show a) Prelude.++ ", " Prelude.++ (show b) Prelude.++ ", " Prelude.++ (show c) Prelude.++ ", " Prelude.++ (show d))

printMat4 :: Mat4 -> IO ()
printMat4 m@(Mat4 a b c d) = do
                    printVec4 a
                    printVec4 b
                    printVec4 c
                    printVec4 d

printMat4d :: Mat4 -> IO ()
printMat4d m@(Mat4 a b c d) = do
                    printVec4d a
                    printVec4d b
                    printVec4d c
                    printVec4d d

gluMakeIdentity :: Mat4
gluMakeIdentity = Mat4 a b c d
    where
        a = Vec4 1.0 0.0 0.0 0.0
        b = Vec4 0.0 1.0 0.0 0.0
        c = Vec4 0.0 0.0 1.0 0.0
        d = Vec4 0.0 0.0 0.0 1.0


gluPerspective :: GLfloat -> GLfloat -> GLfloat -> GLfloat -> Mat4
gluPerspective fovy aspect zNear zFar = Mat4 a b c d
    where
        radians = (fovy / 2 * pi / 180) :: GLfloat
        deltaZ = (zFar - zNear) :: GLfloat
        sine = (sin radians) :: GLfloat
        cotangent = ((cos radians) / sine) :: GLfloat
        negOne = (-1.0 :: GLfloat)
        a = Vec4 ((cotangent / aspect) :: GLfloat) 0.0 0.0 0.0
        b = Vec4 0.0 cotangent 0.0 0.0
        c = Vec4 0.0 0.0 (((zFar + zNear) * (-1.0)) / deltaZ :: GLfloat) negOne
        d = Vec4 0.0 0.0 (-2.0 * zNear * zFar / deltaZ) 0


gluLookAt :: Vec3 -> Vec3 -> Vec3 -> Mat4
gluLookAt eye center up = result
    where
        f@(Vec3 fx fy fz) = normalizeVec3 $ subtractVec3 center eye
        s@(Vec3 sx sy sz) = normalizeVec3 $ crossVec3 f up
        u@(Vec3 ux uy uz) = crossVec3 s f
        -- creating final row values
        ds = (-1.0) * dotVec3 s eye
        du = (-1.0) * dotVec3 u eye
        df = dotVec3 f eye
        -- creating mat4
        ra = Vec4 sx ux ((-1.0) * fx) 0.0
        rb = Vec4 sy uy ((-1.0) * uy) 0.0
        rc = Vec4 sz uz ((-1.0) * fz) 0.0
        rd = Vec4 ds du df 1.0
        result = Mat4 ra rb rc rd


normalizeVec3 :: Vec3 -> Vec3
normalizeVec3 (Vec3 x y z) = Vec3 nx ny nz
    where
        r = sqrt(x * x + y * y + z * z) :: GLfloat
        nx = x / r
        ny = y / r
        nz = z / r


crossVec3 :: Vec3 -> Vec3 -> Vec3
crossVec3 (Vec3 x y z) (Vec3 a b c) = Vec3 cx cy cz
    where
        cx = y * c - z * b
        cy = z * a - x * c
        cz = x * b - y * a


dotVec3 :: Vec3 -> Vec3 -> GLfloat
dotVec3 (Vec3 x y z) (Vec3 a b c) = result
    where
        result = x * a + y * b + z * c


addVec3 :: Vec3 -> Vec3 -> Vec3
addVec3 (Vec3 x y z) (Vec3 a b c) = Vec3 ax ay az
    where
        ax = x + a
        ay = y + b
        az = z + c

addVec4 :: Vec4 -> Vec4 -> Vec4
addVec4 (Vec4 x y z w) (Vec4 a b c d) = Vec4 ax ay az aw
    where
        ax = x + a
        ay = y + b
        az = z + c
        aw = w + d


subtractVec3 :: Vec3 -> Vec3 -> Vec3
subtractVec3 (Vec3 x y z) (Vec3 a b c) = Vec3 sx sy sz
    where
        sx = x - a
        sy = y - b
        sz = z - c


multVec3Scalar :: Vec3 -> GLfloat -> Vec3
multVec3Scalar (Vec3 x y z) n = Vec3 smx smy smz
    where
        smx = x * n
        smy = y * n
        smz = z * n


multVec4Scalar :: Vec4 -> GLfloat -> Vec4
multVec4Scalar (Vec4 x y z w) n = Vec4 smx smy smz smw
    where
        smx = x * n
        smy = y * n
        smz = z * n
        smw = w * n
-- End of vector multiplication


transpose :: Mat4 -> Mat4
transpose (Mat4 (Vec4 a b c d)
                (Vec4 g h i j)
                (Vec4 r s t u)
                (Vec4 x y z w)) = result
    where
        result = Mat4 (Vec4 a g r x)
                      (Vec4 b h s y)
                      (Vec4 c i t z)
                      (Vec4 d j u w)


multMat4s :: Mat4 -> Mat4 -> Mat4
multMat4s m1 m2@(Mat4 a b c d) = result
    where
        c1     = multMat4withVec4 m1 a
        c2     = multMat4withVec4 m1 b
        c3     = multMat4withVec4 m1 c
        c4     = multMat4withVec4 m1 d
        result = Mat4 c1 c2 c3 c4

multMat4withVec4 :: Mat4 -> Vec4 -> Vec4
multMat4withVec4 mat@(Mat4 (Vec4 a b c d)
                           (Vec4 e f g h)
                           (Vec4 i j k l)
                           (Vec4 m n o p))
                 vec@(Vec4 x y z w) = result
    where
        nx     = a * x + b * y + c * z + d * w
        ny     = e * x + f * y + g * z + h * w
        nz     = i * x + j * y + k * z + l * w
        nw     = m * x + n * y + o * z + p * w
        result = Vec4 nx ny nz nw



scaleMat4 :: Mat4 -> Vec3 -> Mat4
scaleMat4 mo v@(Vec3 x y z) = result
    where
        m@(Mat4 a b c d) = transpose mo
        na = multVec4Scalar a x
        nb = multVec4Scalar b y
        nc = multVec4Scalar c z
        nd = d
        result = transpose $ Mat4 na nb nc nd


translateMat4 :: Mat4 -> Vec3 -> Mat4
translateMat4 m@(Mat4 a b c d) (Vec3 x y z) = result
    where
        ta = multVec4Scalar a x
        tb = multVec4Scalar b y
        tc = multVec4Scalar c z
        td = d
        nv = addVec4 td $ addVec4 tc $ addVec4 tb $ ta
        result = Mat4 a b c nv

