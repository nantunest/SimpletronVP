import ForSyDe.Shallow

type RefAngle = Float
type MotorAngle = Float
type GyroAngle = Float
type GyroAngle' = Float
type GyroAngle'' = Float
type PwmDutyCycle = Float
type ControlAngle = Float
type GimbalAngle = Float
type DisturbAngle = Float
type ErrorAngle = Float
type LinearMovement = Float

servoMotorF :: PwmDutyCycle -> MotorAngle
servoMotorF spwm = spwm * 1.8

gimbalF :: DisturbAngle -> MotorAngle -> GimbalAngle
gimbalF d m = d + m

gyro :: GimbalAngle -> GyroAngle
gyro a = a

gimbalDisturbF :: LinearMovement -> DisturbAngle
gimbalDisturbF lm = asin (lm/gimbalArmLength) * (360/(2*pi))
    where gimbalArmLength = 0.2 -- 20cm

-- pCtrl :: ErrorAngle-> PwmSignal
-- pCtrl e = kp*(e/1.8)
--     where kp = 0.25

disturbS :: [LinearMovement]
disturbS =  take 20 [0,0..]
        ++ take 30 [0,0.001..]
        ++ take 50 [0.03, 0.03..]
        ++ take 30 [0.03, 0.031..]
        ++ take 50 [0.06, 0.06..]

-- system :: Int -> PwmSignal
-- system 0 = pCtrl $ gimbal (gimbalDisturb 0) (servoMotor 0)
-- system n = pCtrl (gimbal (gimbalDisturb $ gimbalS!!n) (servoMotor $ system (n-1))) + system (n-1)

diffSY :: Num c => Signal c -> Signal c
diffSY a = zipWithSY (-) a (delaySY 0 a)

sumSY :: Num c => Signal c -> Signal c
sumSY = mooreSY (+) (+0) 0

gimbalDisturb :: Signal LinearMovement -> Signal GimbalAngle
gimbalDisturb = mapSY gimbalDisturbF -- combSY

gimbal :: Signal GimbalAngle -> Signal MotorAngle -> Signal GimbalAngle
gimbal = zipWithSY (+)

gyroP :: Signal GimbalAngle -> Signal GyroAngle''
gyroP = diffSY . diffSY

servoMotor :: Signal PwmDutyCycle -> Signal MotorAngle
servoMotor = mapSY servoMotorF

controlP :: Signal ErrorAngle -> Signal ControlAngle
-- controlP e = zipWithSY (+) (zipWithSY (/) e $ signal $ [1.8..]) (ki * integP e)
--     where kp = 0.2
--           ki = 1
controlP e = zipWithSY (+) (propCtrl e) (integCtrl e)
    where propCtrl = zipWithSY (*) kp
          integCtrl = zipWithSY (*) ki . sumSY
          kp = signal [0.25, 0.25..]
          ki = signal [0.2,0.2..]

-- o = sig/180*100
servoDrive :: Signal ControlAngle -> Signal PwmDutyCycle
servoDrive c = zipWithSY (/) c $ signal [1.8,1.8..]

delayP :: Signal Float -> Signal Float
delayP = delaySY 0

errorInteg :: Signal GyroAngle' -> Signal GyroAngle
errorInteg = mooreSY (+) (+0) 0

errorP :: Signal RefAngle -> Signal GyroAngle -> Signal ErrorAngle
errorP = zipWithSY (-)

sistemP :: Signal LinearMovement -> (Signal ControlAngle,
                                     Signal ErrorAngle,
                                     Signal RefAngle,
                                     Signal GimbalAngle,
                                     Signal MotorAngle,
                                     Signal GimbalAngle,
                                     Signal GyroAngle,
                                     Signal GyroAngle',
                                     Signal GyroAngle'',
                                     Signal PwmDutyCycle)
sistemP sInput = sOutput
    where sOutput = (sControl, sError, sRef, sGimbalDist, sServo, sGimbal, sPos, sVel, sGyro, sServoDrive)
          sControl = controlP sError
          sError = errorP sRef sPos
          sRef = signal $ take (lengthS sInput) [0,0..]
          sPos = sumSY sVel
          sVel = sumSY sGyro
          sGyro = gyroP sGimbal
          sGimbal = gimbal sGimbalDist sServo
          sGimbalDist = gimbalDisturb sInput
          sServo = servoMotor sServoDrive
          sServoDrive = servoDrive sControl


ga = signal disturbS
(sControl, sError, sRef, sGimbalDist, sServo, sGimbal, sPos, sVel, sGyro, sServoDrive)=sistemP ga
pServo = d2aConverter DAlinear 1.0 sServo
pGimbal= d2aConverter DAlinear 1.0 sGimbal
pError = d2aConverter DAlinear 1.0 sError
pGimbalDist = d2aConverter DAlinear 1.0 sGimbalDist
pGyro = d2aConverter DAlinear 1.0 sGyro
pPos = d2aConverter DAlinear 1.0 sPos
pRef = d2aConverter DAlinear 1.0 sRef
pControl = d2aConverter DAlinear 1.0 sControl
pServoDrive = d2aConverter DAlinear 1.0 sServoDrive
pVel = d2aConverter DAlinear 1.0 sVel


plotAll = plotCT' 100 [(pError, "error"),
                       (pControl, "control"),
                       (pPos, "position"),
                       (pGimbalDist, "disturbance"),
                       (pServoDrive, "pwmDC")]

gs :: Signal (SubsigCT LinearMovement)
gs = d2aConverter DAlinear 1.0 $ signal disturbS

plotSingle :: IO String
plotSingle = plotCT' 200 [(pVel, "vel")]

writeSigToFile :: FilePath -> Signal Float -> IO ()
writeSigToFile f s = writeFile f $ concatMap ((show . (++ ",")) . show) (fromSignal s)

sigToStr :: Signal Float -> String
sigToStr s = concatMap ((++ ",") . show . round . (*131)) (fromSignal s)