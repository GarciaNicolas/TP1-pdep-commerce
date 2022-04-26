
--take    ::   Int -> [a] -> [a]  
--drop    ::   Int -> [a] -> [a]
--head    ::   [a] -> a
--elem    ::    a -> [a] -> Bool
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



productoDeLujo :: Producto -> Bool
productoDeLujo unProducto               = elem 'x' (nombre unProducto) || elem 'z' (nombre unProducto)

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


descodiciarProducto :: Producto -> Producto
descodiciarProducto unProducto = (  reverse.(drop  10).reverse.nombre $ unProducto   ,   precio unProducto  )

productoXL :: Producto -> Producto
productoXL unProducto = ( nombre unProducto ++ "XL" , precio unProducto )

versionBarata :: Producto -> Producto
versionBarata unProducto = (    reverse.nombre.descodiciarProducto $ unProducto    ,    precio unProducto   )


 