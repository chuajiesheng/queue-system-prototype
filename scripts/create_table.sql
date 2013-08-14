DROP TABLE queues;
DROP TABLE providers;
DROP TABLE managers;
DROP TABLE users;

CREATE TABLE users (
       id SERIAL PRIMARY KEY,
       email text NOT NULL,
       name text NOT NULL,
       password text NOT NULL
);
INSERT INTO users (email, name, password) VALUES ('js@world.com', 'Jie Sheng', '7c211433f02071597741e6ff5a8ea34789abbf43');

CREATE TABLE managers (
       id SERIAL PRIMARY KEY,
       username text NOT NULL,
       name text NOT NULL,
       password text NOT NULL
);

CREATE TABLE providers (
       id SERIAL PRIMARY KEY,
       name text NOT NULL,
       slot int NOT NULL
);
INSERT INTO providers (name, slot) VALUES
       ('Chams Clinic', 1),
       ('Silver Cross Clinic', 2);
);

CREATE TABLE queues (
       id SERIAL PRIMARY KEY,
       provider_id int REFERENCES providers(id),
       user_id int REFERENCES users(id),
       datetime TIMESTAMP WITH TIME ZONE,
       datetime_called TIMESTAMP WITH TIME ZONE
);
