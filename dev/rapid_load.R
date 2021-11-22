
# Rapid package loading and app running 
devtools::load_all()
run_app()



library(MortalityCauses)
run_app()
plot_map("US")
plot_map("US", zoom = 4)
plot_map("US", zoom = 3)
