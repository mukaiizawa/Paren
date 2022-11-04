; sql module.

;; DDL

(function sql._column->create-query (column)
  (let ((column-name :key primarykey? foreignkey required? type size default-value) column)
    (str "  " column-name " " type (if size (str "(" size ")"))
         (if default-value (str " default " default-value))
         (if (|| required? primarykey?) " not null"))))

(function sql._table->create-query (table)
  (let ((table-name :rest columns) table)
    (str "create table " table-name " (\n"
         (join (map sql._column->create-query columns) ",\n")
         "\n);")))

(function sql._table->constraints (table)
  (let ((table-name :rest columns) table constraints nil)
    (let (pks (keep (f (x) (if (in? :primarykey? x) (car x)))
                    columns))
      (if pks (push! (str "alter table " table-name " add primary key(" (join pks ", ") ");")
                     constraints)))
    (dolist (column columns)
      (let ((column-name :key primarykey? foreignkey required? type size default-value) column)
        (if foreignkey
            (let ((fk-table fk-column) (split (str foreignkey) "."))
              (push! (str "alter table " table-name " add constraints fk_" column-name " "
                          "foreign key(" column-name ") references " fk-table "(" fk-column ");")
                     constraints)))))
    (join (reverse! constraints) "\n")))

(function sql.create-tables (tables)
  ; Returns a list representing a table in the table definition language.
  (join (reject nil?
                (concat
                  (map sql._table->create-query tables)
                  (map sql._table->constraints tables)))
        "\n"))

;; DML

(function sql.sqlstr (x)
  (if (nil? x) "null"
      (string? x) (str "'" x "'")
      (str x)))

(function sql._parse-select (expr)
  (let (parse-expr (f (x)
                     (if (atom? x) x
                         (let ((ope :rest args) x)
                           (str ope "(" (join (map parse-expr args) ", ") ")")))))
    (join (map parse-expr (->list expr)) ", ")))

(function sql._parse-cond (expr)
  (if (atom? expr) (sql.sqlstr expr)
      (let ((ope :rest args) expr)
        (if (= ope 'in)
            (str (car args) " in (" (join (map sql.sqlstr (cdr args)) ",") ")")
            (= ope 'not)
            (str " not (" (sql._parse-cond (car args)) ")")
            (= ope 'is-null)
            (str " " (car args) " is null")
            (= ope 'is-not-null)
            (str " " (car args) " is not null")
            (in? ope '(< > <= >= = <>))
            (str  " " (sql.sqlstr (car args)) " " ope " " (sql.sqlstr (cadr args)))
            (in? ope '(and or))
            (str (sql._parse-cond (car args)) " " ope " ("
                 (if (cddr args) (sql._parse-cond (cons ope (cdr args)))
                     (sql._parse-cond (cadr args)))
                 ")")))))

(function sql._parse-order-by (expr)
  (let (group-by (f (expr acc)
                   (if (nil? expr) (reverse! acc)
                       (if (= (cadr expr) :asc) (group-by (cddr expr) (cons (car expr) acc))
                           (= (cadr expr) :desc) (group-by (cddr expr) (cons (str (car expr) " desc") acc))
                           (group-by (cdr expr) (cons (car expr) acc))))))
    (join (group-by (->list expr) nil) ", ")))

(function sql._parse-value-expr (expr)
  (if (atom? expr) (sql.sqlstr expr)
      (let ((ope :rest args) expr)
        (str (sql._parse-value-expr (car args)) " " ope " " (sql._parse-value-expr (cadr args))))))

(function sql.select (column-names :key from where group-by having order-by)
  ; Returns a select query from the list of specified column names and table names.
  ; If multiple tables are specified, inner join is performed.
  ; In that case, it is necessary to explicitly write the join condition in the where clause.
  (str "select " (sql._parse-select column-names)
       (if from (str " from " (join (->list from) ", ")))
       (if where (str " where" (sql._parse-cond where)))
       (if group-by (str " group by " (join (->list group-by) ", ")))
       (if having (str " having " (sql._parse-cond having)))
       (if order-by (str " order by " (sql._parse-order-by order-by)))
       ";"))

(function sql.insert-into (table-name column-names values)
  ; Returns the insert query string.
  (str "insert into " table-name " (" (join (->list column-names) ", ") ") "
       "values (" (join (map sql.sqlstr values) ", ") ");"))

(function sql.update-set (table-name column-names values :opt cond)
  ; Returns the update query string.
  (let (gen-set (f (column-names vlaues mem)
                  (if (nil? column-names) (.to-s mem)
                      (begin
                        (if (> (.size mem) 0) (.write-bytes ", "))
                        (.write-bytes mem (car column-names))
                        (.write-bytes mem " = ")
                        (.write-bytes mem (sql._parse-value-expr (car values)))
                        (gen-set (cdr column-names) (cdr values) mem)))))
    (str "update " table-name
         " set " (gen-set (->list column-names) (->list values) (.new MemoryStream))
         (if cond (str " where" (sql._parse-cond cond)))
         ";")))

(function sql.delete-from (table-name :opt cond)
  ; Returns the delete query string.
  (str "delete from " table-name
       (if cond (str " where" (sql._parse-cond cond)))
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
    (assert (= (sql.create-tables tables)
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
                       "alter table users add primary key(id);"
                       "alter table products add primary key(id);"
                       "alter table reviews add primary key(user_id, product_id);"
                       "alter table reviews add constraints fk_user_id foreign key(user_id) references users(id);"
                       "alter table reviews add constraints fk_product_id foreign key(product_id) references products(id);") "\n")))
    (assert (= (sql.select 1) "select 1;"))
    (assert (= (sql.select '(id (count *)) :from 'users :group-by 'id)
               "select id, count(*) from users group by id;"))
    (assert (= (sql.select '* :from 'users :order-by 'id)
               "select * from users order by id;"))
    (assert (= (sql.select '* :from 'users :order-by '(id :asc name :desc))
               "select * from users order by id, name desc;"))
    (assert (= (sql.select '(usres.name reviews.text)
                           :from '(users reviews)
                           :where '(and (= users.id reviews.user_id)
                                        (= users.id 3)
                                        (is-not-null reviews.text)))
               (join '("select usres.name, reviews.text "
                       "from users, reviews "
                       "where users.id = reviews.user_id and ( users.id = 3 and ( reviews.text is not null));"))))
    (assert (= (sql.insert-into 'users '(id name) '(0 "alice"))
               "insert into users (id, name) values (0, 'alice');"))
    (assert (= (sql.update-set 'products '(price) '((+ price 1000)) '(= id 10))
               "update products set price = price + 1000 where id = 10;"))
    (assert (= (sql.delete-from 'users)
               "delete from users;"))
    (assert (= (sql.delete-from 'products '(= id 3))
               "delete from products where id = 3;"))))
