package fr.cnrs.liris.scars.database.epinions.database

//TODO: gérer la date des ratings
case class Query(
  categoriesQuery: String,
  parentCategoriesQuery: String,
  categoriesWithParentQuery: String,
  actorsQuery: String,
  expertsQuery: String,
  itemsQuery: String,
  trustsQuery: String,
  similaritiesQuery: String,
  reviewsQuery: String,
  evalReviewsQuery: String
)

object EpinionsQuery extends Query(
  categoriesQuery = "SELECT idcategory, name, parent FROM Category ORDER BY deep ASC",
  parentCategoriesQuery = "SELECT idcategory, name FROM Category WHERE parent is null",
  categoriesWithParentQuery = "SELECT idcategory, name FROM Category WHERE parent = ?",
  actorsQuery = "SELECT iduser, rank FROM User",
  expertsQuery = "SELECT iduser, idcategory, expertise FROM Expertise",
  itemsQuery = "SELECT idproduct, name, idcategory FROM Product",
  trustsQuery = "SELECT iduser, idtrusted, trust FROM Trust",
  similaritiesQuery = "SELECT iduser, idsimilar, similarity FROM Similarity",
  reviewsQuery = "SELECT idreview, iduser, idproduct, rating, review_rating FROM Review",
  evalReviewsQuery = "SELECT idreview, iduser, idproduct, rating, review_rating FROM Review"
)

object AppoliciousQuery extends Query(
  categoriesQuery = "SELECT id, name, NULL FROM categories",
  parentCategoriesQuery = "SELECT id, name, NULL FROM categories",
  categoriesWithParentQuery = "SELECT id, name FROM categories WHERE 1 = 2",
  actorsQuery = "SELECT id, NULL FROM users",
  expertsQuery = "SELECT NULL, NULL, NULL FROM users WHERE 1 = 2",
  itemsQuery = """SELECT a.id, a.name, ac.id_category 
                  FROM applications a 
                  LEFT JOIN application_categories ac on (a.id = ac.id_application)""",
  trustsQuery = "SELECT id_follower, id_followed, 1 FROM followers",
  similaritiesQuery = "SELECT id_user, id_similar, similarity FROM user_similarities",
  reviewsQuery = """SELECT id, id_user, id_application, rating, concat(helpful, "/", helpful_total)
                    FROM `reviews` 
                    UNION
                    SELECT id, id_user, id_application, rating, NULL 
                    FROM `ratings`
                    WHERE rating IS NOT NULL""",
  evalReviewsQuery = """SELECT id, id_user, id_application, rating, concat(helpful, "/", helpful_total)
                        FROM `reviews` 
                        UNION
                        SELECT id, id_user, id_application, rating, NULL 
                        FROM `ratings`
                        WHERE rating IS NOT NULL"""
)

