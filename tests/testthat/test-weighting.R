
test_that(
  "product_limit matches survival", {
   forall(gen.bind(gen_rcens, gen.int(100)),  function(x){ compare_km(x) } )
  }
)

