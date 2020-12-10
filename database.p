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
    (string  "	" column-name " " (mem->str type) (if size (string "(" size ")"))
             (if default-value (string " default " default-value))
             (if (|| required? primarykey?) " not null"))))

(function table->create-query (table)
  (let ((table-name :rest columns) table)
    (string "create table " table-name " (\n"
            (join (map column->create-query columns) ",\n")
            "\n);\n\n")))

(function table->constraints (table)
  (let (primarykeys (select-table-columns table :primarykey?)
                    foreignkeys (select-table-columns table :foreignkey))
    (join
      (append
        (if primarykeys
            (list
              (string "alter table " (car table) " add primary key (" (join (map car primarykeys) ", ") ");\n")))
        (map (f (x)
               (let (fk (assoc (cdr x) :foreignkey) (fk-table fk-column) (x.y->pairs fk))
                 (string "alter table " (car table) " add constraints fk_" (car x) " foreign key(" (car x) ") references " fk-table "(" fk-column ");\n")))
             foreignkeys)))))

(function create-tables (tables)
  ; Returns a list representing a table in the table definition language.
  (join (except nil?
                (append
                  (map table->create-query tables)
                  (map table->constraints tables)))))

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
  (string "select " (join column-names ", ")
          " from "(join table-names ", ")
          (if cond (string " where" (parse-condition cond)))
          ";"))

(function insert-into (table-name column-names values)
  (string "insert into " table-name " (" (join column-names ", ") ") "
          "values (" (join (map ->sql-str values) ", ") ");\n"))

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
    (write-line (create-tables tables))
    (write-line (select-from '(users reviews)
                             '(usres.name reviews.text)
                             '(and (= users.id reviews.user_id)
                                   (= users.id 3)
                                   (is-not-null reviews.text))))
    (write-line (insert-into 'users '(:id :name) '(0 "alis")))
    (assert (memeq? (delete-from 'users) "delete from users;"))))
