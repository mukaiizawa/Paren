; database module.

;; DDL

(function x.y->pairs (x.y)
  (let (x.y (mem->str x.y) i (memmem x.y "."))
    (list (submem x.y 0 i)
          (submem x.y (++ i)))))

(function select-table-columns (table key)
  (select (f (x) (nilable-assoc (cdr x) key))
          (cdr table)))

(function column->create-query (column)
  (let ((column-name :key primarykey? foreignkey required? type size default-value) column)
    (string  "  " column-name " " (mem->str type) (if size (string "(" size ")"))
             (if default-value (string " default " default-value))
             (if (|| required? primarykey?) " not null"))))

(function table->create-query (table)
  (let ((table-name :rest columns) table)
    (string "create table " table-name " (\n"
            (join (map column->create-query columns) ",\n")
            "\n);")))

(function table->constraints (table)
  (let (primarykeys (select-table-columns table :primarykey?)
                    foreignkeys (select-table-columns table :foreignkey))
    (join
      (append
        (if primarykeys
            (list
              (string "alter table " (car table) " add primary key (" (join (map car primarykeys) ", ") ");")))
        (map (f (x)
               (let (fk (assoc (cdr x) :foreignkey) (fk-table fk-column) (x.y->pairs fk))
                 (string "alter table " (car table) " add constraints fk_" (car x) " foreign key(" (car x) ") references " fk-table "(" fk-column ");")))
             foreignkeys))
      "\n")))

(function create-tables (tables)
  ; Returns a list representing a table in the table definition language.
  (join (except nil?
                (append
                  (map table->create-query tables)
                  (map table->constraints tables)))
        "\n"))

;; DML

(function ->sql-str (x)
  (if (nil? x) "null"
      (string? x) (string "'" x "'")
      (string x)))

(function parse-condition (expr)
  ;; expr = '(' { unary-expr | binary-expr | multinomial-expr } ')' | value
  ;; unary-expr = { 'not' |  'is-null' | 'is-not-null' } expr
  ;; binary-expr = { '<' | '>' | '<=' | '>=' | '=' | '<>' | 'between' }  expr expr
  ;; multinomial-expr = { 'in' | 'between' | 'and' | 'or' } expr ...
  (if (atom? expr) (->sql-str expr)
      (let ((ope :rest args) expr)
        (switch ope
          in (string ope " in (" (join (map ->sql-str (cdr args)) ",") ")")
          not (string " not (" (parse-condition (car args)) ")")
          is-null (string " " (car args) " is null")
          is-not-null (string " " (car args) " is not null")
          (< > <= >= = <>) (string  " " (->sql-str (car args)) " " ope " " (->sql-str (cadr args)))
          (and or) (string  (parse-condition (car args)) " " ope " ("
                            (if (cddr args) (parse-condition (cons ope (cdr args)))
                                (parse-condition (cadr args)))
                            ")")))))

(function select-from (table-names column-names :opt cond)
  (string "select " (join (->list column-names) ", ")
          " from "(join (->list table-names) ", ")
          (if cond (string " where" (parse-condition cond)))
          ";"))

(function insert-into (table-name column-names values)
  (string "insert into " table-name " (" (join (->list column-names) ", ") ") "
          "values (" (join (map ->sql-str values) ", ") ");"))

(function update-set (table-name column-names values :opt cond)
  (string "update " table-name
          " set " ))

(function delete-from (table-name :opt cond)
  (string "delete from " table-name
          (if cond (string " where" (parse-condition cond)))
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
    (assert (memeq? (create-tables tables)
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
    (assert (memeq? (select-from '(users reviews)
                                 '(usres.name reviews.text)
                                 '(and (= users.id reviews.user_id)
                                       (= users.id 3)
                                       (is-not-null reviews.text)))
                    (join '("select usres.name, reviews.text "
                            "from users, reviews "
                            "where users.id = reviews.user_id and ( users.id = 3 and ( reviews.text is not null));"))))
    (assert (memeq?  (insert-into 'users '(:id :name) '(0 "alice"))
                     "insert into users (id, name) values (0, 'alice');"))
    (assert (memeq? (delete-from 'users) "delete from users;"))))
