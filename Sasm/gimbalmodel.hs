
type MotorAngle = Float
type GyroAngle = Float
type PwmSignal = Integer
type GimbalAngle = Float
type LinearMovement = Float

servoMotor :: PwmSignal -> MotorAngle
servoMotor spwm = (fromInteger spwm / 100) * 360.0

gyro :: GimbalAngle -> MotorAngle -> GyroAngle
gyro ga ma = ga - ma

gimbal :: LinearMovement -> GimbalAngle
gimbal lm = asin(lm/gimbalArmLength) * (360/(2*pi))
    where gimbalArmLength = 0.1 -- 10cm
