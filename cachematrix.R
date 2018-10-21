makeCacheMatrix  <-  function ( original.matrix  =  matrix ()) {
  

  if ( ! is.matrix ( original.matrix )) {
    parada ( " Por favor, da una matriz " )
  }
  
  inverted.matrix  <-  NULL
  
  establecer  <-  función ( y ) {
    original.matrix  << -  y
    inverted.matrix  << -  NULL
  }

  obtener  <-  function () original.matrix
  set.inverse  <-  función ( resolver ) inverted.matrix  << -  resolver
  get.inverse  <-  function () inverted.matrix
  
  lista (
    set  =  set ,
    get  =  get ,
    set.inverse  =  set.inverse ,
    get.inverse  =  get.inverse )
  
}



cacheSolve  <-  function ( cacheable.matrix , ... ) {
  inverted.matrix  <-  cacheable.matrix $ get.inverse ()
  if ( ! is.null ( inverted.matrix )) {
    mensaje ( " Obtención de matriz inversa en caché " )
    retorno ( inverted.matrix )
  }
  
  matrix.to.inverse  <-  cacheable.matrix $ get ()
  inverted.matrix  <- resolver ( matrix.to.inverse )
  cacheable.matrix $ set.inverse ( inverted.matrix )
  matriz invertida
  
}