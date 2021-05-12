; database module.

;; DDL

(function db.select-table-columns (table key)
  (select (f (x) (assoc (cdr x) key))
          (cdr table)))

(function db.column->create-query (column)
  (let ((column-name :key primarykey? foreignkey required? type size default-value) column)
    (str "  " column-name " " type (if size (str "(" size ")"))
         (if default-value (str " default " default-value))
         (if (|| required? primarykey?) " not null"))))

(function db.table->create-query (table)
  (let ((table-name :rest columns) table)
    (str "create table " table-name " (\n"
         (join (map db.column->create-query columns) ",\n")
         "\n);")))

(function db.table->constraints (table)
  (let (primarykeys (db.select-table-columns table :primarykey?)
                    foreignkeys (db.select-table-columns table :foreignkey))
    (join
      (append
        (if primarykeys (list (str "alter table " (car table) " "
                                   "add primary key (" (join (map car primarykeys) ", ") ");")))
        (map (f (x)
               (let (fk (assoc (cdr x) :foreignkey) (fk-table fk-column) (split (str fk) "."))
                 (str "alter table " (car table) " "
                      "add constraints fk_" (car x) " "
                      "foreign key(" (car x) ") "
                      "references " fk-table "(" fk-column ");")))
             foreignkeys))
      "\n")))

(function db.create-tables (tables)
  ; Returns a list representing a table in the table definition language.
  (join (except nil?
                (append
                  (map db.table->create-query tables)
                  (map db.table->constraints tables)))
        "\n"))

;; DML

(function db.sqlstr (x)
  (if (nil? x) "null"
      (string? x) (str "'" x "'")
      (str x)))

(function db.parse-select (expr)
  (let (parse-expr (f (x)
                     (if (atom? x) x
                         (let ((ope :rest args) x)
                           (str ope "(" (join (map parse-expr args) ", ") ")")))))
    (join (map parse-expr (->list expr)) ", ")))

(function db.parse-cond (expr)
  (if (atom? expr) (db.sqlstr expr)
      (let ((ope :rest args) expr)
        (if (= ope 'in)
            (str (car args) " in (" (join (map db.sqlstr (cdr args)) ",") ")")
            (= ope 'not)
            (str " not (" (db.parse-cond (car args)) ")")
            (= ope 'is-null)
            (str " " (car args) " is null")
            (= ope 'is-not-null)
            (str " " (car args) " is not null")
            (in? ope '(< > <= >= = <>))
            (str  " " (db.sqlstr (car args)) " " ope " " (db.sqlstr (cadr args)))
            (in? ope '(and or))
            (str (db.parse-cond (car args)) " " ope " ("
                 (if (cddr args) (db.parse-cond (cons ope (cdr args)))
                     (db.parse-cond (cadr args)))
                 ")")))))

(function db.parse-order-by (expr)
  (let (group-by (f (expr acc)
                   (if (nil? expr) (reverse! acc)
                       (if (= (cadr expr) :asc) (group-by (cddr expr) (cons (car expr) acc))
                           (= (cadr expr) :desc) (group-by (cddr expr) (cons (str (car expr) " desc") acc))
                           (group-by (cdr expr) (cons (car expr) acc))))))
    (join (group-by (->list expr) nil) ", ")))

(function db.parse-value-expr (expr)
  (if (atom? expr) (db.sqlstr expr)
      (let ((ope :rest args) expr)
        (str (db.parse-value-expr (car args)) " " ope " " (db.parse-value-expr (cadr args))))))

(function db.select-from (column-names table-names :key where group-by having order-by)
  ; Returns a select query from the list of specified column names and table names.
  ; If multiple tables are specified, inner join is performed.
  ; In that case, it is necessary to explicitly write the join condition in the where clause.
  (str "select " (db.parse-select column-names)
       " from "(join (->list table-names) ", ")
       (if where (str " where" (db.parse-cond where)))
       (if group-by (str " group by " (join (->list group-by) ", ")))
       (if having (str " having " (db.parse-cond having)))
       (if order-by (str " order by " (db.parse-order-by order-by)))
       ";"))

(function db.insert-into (table-name column-names values)
  ; Returns the insert query string.
  (str "insert into " table-name " (" (join (->list column-names) ", ") ") "
       "values (" (join (map db.sqlstr values) ", ") ");"))

(function db.update-set (table-name column-names values :opt cond)
  ; Returns the update query string.
  (let (gen-set (f (column-names vlaues mem)
                  (if (nil? column-names) (.to-s mem)
                      (begin
                        (if (> (.size mem) 0) (.write-bytes ", "))
                        (.write-bytes mem (car column-names))
                        (.write-bytes mem " = ")
                        (.write-bytes mem (db.parse-value-expr (car values)))
                        (gen-set (cdr column-names) (cdr values) mem)))))
    (str "update " table-name
         " set " (gen-set (->list column-names) (->list values) (.new MemoryStream))
         (if cond (str " where" (db.parse-cond cond)))
         ";")))

(function db.delete-from (table-name :opt cond)
  ; Returns the delete query string.
  (str "delete from " table-name
       (if cond (str " where" (db.parse-cond cond)))
       ";"))

(function! main (args)
  (let (tables '((users
                   (id :primarykey? true :type "number(10)")
                   (name :required? true :type "varchar(10)"))
                 (products
                   (id :primarykey? true :type "number(10)")
                   (name :required? true :type "varchar(10)")
                   (price :required? true :type "number(10)"))
                 (reviews
                   (user_id :primarykey? true :foreignkey users.id :type "number(10)")
                   (product_id :primarykey? true :foreignkey products.id :type "number(10)")
                   (text :required? true :type "varchar(1000)"))))
    (assert (= (db.create-tables tables)
               (join '("create table users ("
                       "  id number(10) not null,"
                       "  name varchar(10) not null"
                       ");"
                       "create table products ("
                       "  id number(10) not null,"
                       "  name varchar(10) not null,"
                       "  price number(10) not null"
                       ");"
                       "create table reviews ("
                       "  user_id number(10) not null,"
                       "  product_id number(10) not null,"
                       "  text varchar(1000) not null"
                       ");"
                       "alter table users add primary key (id);"
                       "alter table products add primary key (id);"
                       "alter table reviews add primary key (user_id, product_id);"
                       "alter table reviews add constraints fk_user_id foreign key(user_id) references users(id);"
                       "alter table reviews add constraints fk_product_id foreign key(product_id) references products(id);") "\n")))
    (assert (= (db.select-from '(usres.name reviews.text) '(users reviews)
                               :where '(and (= users.id reviews.user_id)
                                            (= users.id 3)
                                            (is-not-null reviews.text)))
               (join '("select usres.name, reviews.text "
                       "from users, reviews "
                       "where users.id = reviews.user_id and ( users.id = 3 and ( reviews.text is not null));"))))
    (assert (= (db.select-from '(id (count *)) 'users :group-by 'id)
               "select id, count(*) from users group by id;"))
    (assert (= (db.select-from '* 'users :order-by 'id)
               "select * from users order by id;"))
    (assert (= (db.select-from '* 'users :order-by '(id :asc name :desc))
               "select * from users order by id, name desc;"))
    (assert (= (db.insert-into 'users '(:id :name) '(0 "alice"))
               "insert into users (id, name) values (0, 'alice');"))
    (assert (= (db.update-set 'products '(price) '((+ price 1000)) '(= id 10))
               "update products set price = price + 1000 where id = 10;"))
    (assert (= (db.delete-from 'users)
               "delete from users;"))
    (assert (= (db.delete-from 'products '(= id 3))
               "delete from products where id = 3;"))))
