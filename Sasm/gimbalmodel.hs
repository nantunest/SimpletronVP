import ForSyDe.Shallow

type RefAngle = Float
type MotorAngle = Float
type GyroAngle = Float
type GyroAngle' = Float
type PwmSignal = Float
type GimbalAngle = Float
type DisturbAngle = Float
type ErrorAngle = Float
type LinearMovement = Float

servoMotor :: PwmSignal -> MotorAngle
servoMotor spwm = spwm * 1.8

gimbal :: DisturbAngle -> MotorAngle -> GimbalAngle
gimbal ga ma = ga + ma

gyro :: GimbalAngle -> GyroAngle
gyro a = a

gimbalDisturb :: LinearMovement -> DisturbAngle
gimbalDisturb lm = asin (lm/gimbalArmLength) * (360/(2*pi))
    where gimbalArmLength = 0.2 -- 20cm

control :: ErrorAngle-> PwmSignal
control e = kp*(e/1.8)
    where kp = 0.2

gimbalS :: [LinearMovement]
gimbalS = take 20 [0.03, 0.03..] ++ take 30 [0.06, 0.06..]

system :: Int -> PwmSignal
system 0 = control $ gimbal (gimbalDisturb 0) (servoMotor 0)
system n = control (gimbal (gimbalDisturb $ gimbalS!!n) (servoMotor $ system (n-1))) + system (n-1)

gimbalDisturbP :: Signal LinearMovement -> Signal GimbalAngle
gimbalDisturbP = mapSY gimbalDisturb -- combSY

gimbalP :: Signal GimbalAngle -> Signal MotorAngle -> Signal GimbalAngle
gimbalP = zipWithSY gimbal

gyroP :: Signal GimbalAngle -> Signal GyroAngle'
gyroP a = zipWithSY (-) a (delaySY 0 a)

servoMotorP :: Signal PwmSignal -> Signal MotorAngle
servoMotorP = mapSY servoMotor

controlP :: Signal ErrorAngle -> Signal PwmSignal
controlP = mapSY control

delayP :: Signal Float -> Signal Float
delayP = delaySY 0

errorInteg :: Signal GyroAngle' -> Signal GyroAngle
errorInteg = mooreSY (+) (+0) 0

errorP :: Signal RefAngle -> Signal GyroAngle -> Signal ErrorAngle
errorP r g = zipWithSY (-) r (errorInteg g)

sistemP sInput = sOutput
    where sOutput = (iControl, sControl, sError, sRef, sGimbalDist, sServo, sGimbal, sGyro)
          sControl = controlP sError
          sError = errorP sRef sGyro
          sRef = signal $ take (lengthS sInput) [0,0..]
          sGyro = gyroP sGimbal
          sGimbal = gimbalP sGimbalDist sServo
          sGimbalDist = gimbalDisturbP sInput
          sServo = servoMotorP iControl
          iControl = mooreSY (+) (+0) 0 sControl


ga = signal gimbalS
(iControl, sControl, sError, sRef, sGimbalDist, sServo, sGimbal, sGyro)=sistemP ga
pServo = d2aConverter DAlinear 1.0 sServo
pGimbal= d2aConverter DAlinear 1.0 sGimbal
pError = d2aConverter DAlinear 1.0 sError
pGimbalDist = d2aConverter DAlinear 1.0 sGimbalDist
pGyro = d2aConverter DAlinear 1.0 sGyro

plotAll = plotCT' 100 [(pServo, "servo"), 
                       (pGimbal, "gimbal"),
                       (pGyro, "gyro"),
                       (pError, "error")
                       ]

plotGimbal = plotCT' 100 [(pGimbal, "gimbal")]