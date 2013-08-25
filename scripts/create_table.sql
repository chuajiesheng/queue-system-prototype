DROP TABLE queues;
DROP TABLE managers;
DROP TABLE providers;
DROP TABLE users;

CREATE TABLE users (
       id SERIAL PRIMARY KEY,
       email text NOT NULL,
       name text NOT NULL,
       password text NOT NULL
);
INSERT INTO users (email, name, password) VALUES
       ('js@world.com', 'Jie Sheng', '7c211433f02071597741e6ff5a8ea34789abbf43'),
       ('js@hello.com', 'Jason', 'aaf4c61ddcc5e8a2dabede0f3b482cd9aea9434d');

CREATE TABLE providers (
       id SERIAL PRIMARY KEY,
       name text NOT NULL
);
INSERT INTO providers (name) VALUES
       ('Chams Clinic'),
       ('Silver Cross Clinic');

CREATE TABLE managers (
       id SERIAL PRIMARY KEY,
       username text NOT NULL,
       name text NOT NULL,
       password text NOT NULL,
       provider_id int REFERENCES providers(id)
);
INSERT INTO managers (username, name, password, provider_id) VALUES
       ('chams', 'Dr Chams', '053ad7fc22c565a5f386774e51b8f7782799d0aa', 1);

CREATE TABLE queues (
       id SERIAL PRIMARY KEY,
       provider_id int REFERENCES providers(id),
       user_id int REFERENCES users(id),
       datetime TIMESTAMP WITH TIME ZONE,
       datetime_arrived TIMESTAMP WITH TIME ZONE NULL,
       datetime_called TIMESTAMP WITH TIME ZONE NULL
);
