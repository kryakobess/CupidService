CREATE TABLE USER (
  id integer PRIMARY KEY AUTOINCREMENT,
  username varchar UNIQUE,
  password varchar
);

CREATE TABLE USER_FEATURES(
    id integer PRIMARY KEY references User(id),
    age integer,
    gender integer,
    sports integer,
    tv_sports integer,
    exercise integer,
    gaming integer,
    clubbing integer,
    reading integer,
    shopping integer,
    yoga integer,
    museums integer,
    go_out integer,
    art integer,
    hiking integer,
    tv integer,
    theater integer,
    movies integer,
    concerts integer,
    music integer,
    attr integer,
    sinc integer,
    intel integer,
    fun integer,
    amb integer,
    shar integer,
    attr_a integer,
    sinc_a integer,
    intel_a integer,
    fun_a integer,
    amb_a integer,
    shar_a integer,
    imprace integer,
    imprelig integer
);

CREATE TABLE USER_LIKE(
  from_id integer,
  to_id integer
);