
--take    ::   Int -> [a] -> [a]  
--drop    ::   Int -> [a] -> [a]
--head    ::   [a] -> a
--elem    ::   Eq a => a -> [a] -> Bool
--reverse ::   [a] -> [a]

type Producto = (String, Float)

nombre :: Producto -> String
nombre producto = fst producto
precio :: Producto -> Float
precio producto = snd producto



aplicarDescuento :: Float -> Float -> Float
aplicarDescuento unPrecio descuento                  = unPrecio * ((100-descuento)/100)  

aplicarCostoDeEnvio :: Float -> Float -> Float
aplicarCostoDeEnvio unPrecio costoEnvio              = costoEnvio + unPrecio

precioTotal :: Producto -> Float -> Float -> Float -> Float
precioTotal unProducto cantidad descuento costoEnvio = (aplicarDescuento (precio unProducto) descuento * cantidad) `aplicarCostoDeEnvio` costoEnvio 

nombreDeLujo :: String -> Bool
nombreDeLujo unNombre = elem 'x' unNombre || elem 'z' unNombre

productoDeLujo :: Producto -> Bool
productoDeLujo unProducto               = nombreDeLujo.nombre $ unProducto

vocales :: String
vocales = "aeiouAEIOU"
productoCorriente :: Producto  -> Bool
productoCorriente unProducto            = head (nombre unProducto) `elem` vocales 

productoCodiciado :: Producto -> Bool
productoCodiciado unProducto            = length (nombre unProducto) >= 10

productoDeElite :: Producto -> Bool 
productoDeElite unProducto =  productoDeLujo unProducto && productoCodiciado unProducto && not (productoCorriente unProducto)


entregaSencilla :: String -> Bool
entregaSencilla dia = even.length $ dia

modificarNombre :: (String -> String) -> Producto ->  String
modificarNombre unaFuncion (unNombre, _) = unaFuncion unNombre

descodiciar :: String -> String
descodiciar unNombre = reverse.(drop  10).reverse $ unNombre

abaratar :: String -> String
abaratar unNombre = reverse.descodiciar $ unNombre

descodiciarProducto :: Producto -> Producto
descodiciarProducto unProducto = (  modificarNombre descodiciar unProducto  ,   precio unProducto  )

productoXL :: Producto -> Producto
productoXL unProducto = ( modificarNombre (++"XL") unProducto , precio unProducto )

versionBarata :: Producto -> Producto
versionBarata unProducto = (    modificarNombre abaratar unProducto    ,    precio unProducto   )


 