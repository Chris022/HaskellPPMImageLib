import Image

main = do
    let img = newImage (100,100) (255,255,255)
    let circleO = circle (50,50) 10
    let rectO = rect (50,50) 20 20
    let obj = difference rectO circleO
    let imgCircle = draw obj (0,0,0) img
    saveImage "out.ppm" imgCircle