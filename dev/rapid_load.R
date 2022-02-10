
# Rapid package loading and app running 
devtools::load_all()
run_app()





library(lemur)
run_app()
plot_map("US")

suppressWarnings(plot_map("TUNISIA", zoom = 2))
