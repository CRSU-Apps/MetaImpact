
# Regular expression explanation:
# ^ = Start of string
# (?i) = Ignore case for matching
# (\\.([0-9]+))? = Optional group of full stop, followed by at least one digit
# $ = End of string
# (.+) = Group of at least one character
meta_impact_column_names <- data.frame() %>%
  
  # Continuous data
  rbind(data.frame(name = "Study", pattern = "^(?i)Study(\\.([0-9]+))?$", replacement ="Study\\1")) %>%
  rbind(data.frame(name = "T", pattern = "^(?i)T(\\.([0-9]+))?$", replacement ="T\\1")) %>%
  rbind(data.frame(name = "N", pattern = "^(?i)N(\\.([0-9]+))?$", replacement ="N\\1")) %>%
  rbind(data.frame(name = "Mean", pattern = "^(?i)Mean(\\.([0-9]+))?$", replacement ="Mean\\1")) %>%
  rbind(data.frame(name = "SD", pattern = "^(?i)SD(\\.([0-9]+))?$", replacement ="SD\\1")) %>%
  
  # Binary data
  rbind(data.frame(name = "R", pattern = "^(?i)R(\\.([0-9]+))?$", replacement ="R\\1"))
