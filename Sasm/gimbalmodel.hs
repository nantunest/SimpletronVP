{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use camelCase" #-}

type MotorAngle = Float
type GyroAngle = Float
type PwmSignal = Float
type GimbalAngle = Float
type AngleError = Float
type LinearMovement = Float

servoMotor :: PwmSignal -> MotorAngle
servoMotor spwm = (spwm / 100) * 180.0

gyro :: GimbalAngle -> MotorAngle -> GyroAngle
gyro ga ma = ga - ma

gimbal :: LinearMovement -> GimbalAngle
gimbal lm = asin (lm/gimbalArmLength) * (360/(2*pi))
    where gimbalArmLength = 0.2 -- 10cm

control :: AngleError -> PwmSignal
control e = e/1.8

gimbalS :: [Float]
gimbalS = [0, 0.01, 0.01, 0.01, 0.01, 0.02, 0.02, 0.02, 0.02, 0.02, 0.01, 0, -0.03, 0.18]

system :: Int -> PwmSignal
system 0 = control $ gyro (gimbal 0) (servoMotor 0)
system n = control (gyro (gimbal $ gimbalS!!n) (servoMotor $ system (n-1))) + system (n-1)
