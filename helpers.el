;;; helpers.el -*- lexical-binding: t; -*-

;; Example usage: place the cursor on the line you want to process and run M-x parse-and-generate-sql-update

(defun parse-and-generate-sql-update ()
  "Reads a string in the specified format and generates an SQL UPDATE statement."
  (interactive)
  (save-excursion
    (beginning-of-line)
    (let* ((line (thing-at-point 'line t))
           (parts (split-string line "','"))
           (tehtavaluokantunnus (string-trim (nth 1 parts) "'" "'"))
           (tehtavankuvaus (string-trim (nth 2 parts) "'" "'"))
           (sql (format "UPDATE TEHTAVALUOKKA\n   SET TEHTAVANKUVAUS = '%s'\n WHERE TEHTAVALUOKANTUNNUS = '%s';"
                         tehtavankuvaus tehtavaluokantunnus)))
      (if (and tehtavankuvaus tehtavaluokantunnus)
          (progn
            (delete-region (line-beginning-position) (line-end-position))
            (insert sql))
        (message parts)))))
