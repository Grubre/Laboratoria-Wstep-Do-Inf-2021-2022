1: SHOW FULL TABLES;
2: SELECT title FROM film WHERE length>120;
3: SELECT title FROM film WHERE rating='G' ORDER BY length LIMIT 4;
4: SELECT film.title, language.name FROM film, language WHERE film.language_id=language.language_id AND film.description LIKE '%Drama%';
5: SELECT title FROM film, (SELECT film_id FROM film_category, category WHERE film_category.category_id=category.category_id AND category.name='Family') as cat WHERE cat.film_id=film.film_id AND film.description LIKE '%Documentary%';
6: SELECT title FROM film, (SELECT film_id FROM film_category, category WHERE film_category.category_id=category.category_id AND category.name='Children') as cat WHERE cat.film_id=film.film_id AND film.rating NOT LIKE 'G';
7: SELECT rating, COUNT(rating) FROM film GROUP BY rating;
8: SELECT DISTINCT title FROM inventory, rental, film WHERE rental_date BETWEEN '2005-05-31' AND '2005-06-15' AND rental.inventory_id=inventory.inventory_id AND inventory.film_id=film.film_id ORDER BY title;
9: SELECT DISTINCT first_name, last_name AS nam_sur FROM actor, film_actor WHERE actor.actor_id=film_actor.actor_id AND film_actor.film_id IN (SELECT film_id FROM film WHERE FIND_IN_SET('Deleted Scenes', special_features)>0) GROUP BY actor.actor_id;
10: SELECT DISTINCT first_name, last_name FROM rental, payment, customer WHERE rental.rental_id=payment.rental_id AND rental.staff_id NOT LIKE payment.staff_id AND payment.customer_id=customer.customer_id GROUP BY payment.customer_id;
11:
CREATE TEMPORARY TABLE mary AS SELECT customer_id FROM customer WHERE email LIKE "MARY.SMITH@sakilacustomer.org";
SELECT rental.customer_id, COUNT(rental.customer_id) FROM mary,rental GROUP BY rental.customer_id HAVING COUNT(rental.customer_id) > (SELECT COUNT(rental.rental_id) FROM rental,mary WHERE rental.customer_id = mary.customer_id);
12: SELECT a1.actor_id, a2.actor_id, COUNT(a1.actor_id) FROM film_actor as a1, film_actor as a2 WHERE a1.film_id = a2.film_id AND a1.actor_id > a2.actor_id GROUP BY a1.actor_id,a2.actor_id HAVING COUNT(a1.actor_id) > 1 ORDER BY a1.actor_id;
13: SELECT first_name, last_name FROM actor_info WHERE film_info NOT LIKE "%: C%" AND film_info NOT LIKE "%, C%";
14:
CREATE TEMPORARY TABLE my_film_cats AS SELECT film_id, film_category.category_id, category.name FROM film_category, category WHERE category.category_id=film_category.category_id;
SELECT actor.last_name FROM film_actor AS fa, my_film_cats, actor WHERE actor.actor_id=fa.actor_id AND fa.film_id=my_film_cats.film_id AND (my_film_cats.name='Action' OR my_film_cats.name='HORROR') GROUP BY fa.actor_id HAVING COUNT(CASE my_film_cats.name when 'Action' THEN 1 ELSE NULL END) > COUNT(CASE my_film_cats.name when 'Horror' THEN 1 ELSE NULL END);
15: SELECT customer_id FROM payment GROUP BY customer_id HAVING AVG(amount) < (SELECT AVG(amount) FROM payment WHERE payment_date LIKE "2005-07-30 %");
16: UPDATE film SET language_id = (SELECT language_id FROM language WHERE name='Italian') WHERE title='YOUNG LANGUAGE';
17: UPDATE film SET language_id = (SELECT language_id FROM language WHERE name='Spanish') WHERE film_id IN (SELECT film_id FROM film_actor WHERE actor_id LIKE (SELECT actor_id FROM actor WHERE first_name='ED' AND last_name='CHASE'));
18: UPDATE language JOIN (SELECT language_id, COUNT(*) AS cnt FROM film GROUP BY language_id) as lng ON language.language_id=lng.language_id SET language.films_no=lng.cnt;
19: ALTER TABLE film DROP COLUMN release_year;
