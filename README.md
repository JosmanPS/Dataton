# Datatón 2014

With this project we won the national contest "Datatón" 2014 organized by the Presidential Office in Mexico. It was developed by Omar Trejo (@otrenav) and Luis Román (@luwiss) during a two weak period in June 2014.

# General ideas

All of the results were produced for the Zapopan area in Jalisco because that the origin of the data provided in the contest. There are two main ideas:

## Crime prediction

For this part we analyzed various socio-economical variables that were available in the databases available for the contest, such as schools, shopping centers, hospitals, and others, and we used _stochastic gradient boosting_ to learn what characteristics lead to crime. All these observations came with a geographical references that we used to map our results and provide a profile that would help reduce crime.

## Real time response

For this part we developed a system that analyses a Twitter feed produced by a geographical zone (in this case Zapopan) and look for words that are linked to crime, such as _secuestro_, _robo_ and _delito_ (kidnapping, stealing and crime, in english) and then provide those people with an automatic response that informs them of the closest hospital available, the route and the phone number. The route provided is optimal and it is taken through the Google Directions API so that incorporates traffic status.

# Note

All of the files used for the project are here, although some are not finished yet (we are still improving this system).

