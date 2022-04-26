select      "Aircraft Manufacturer" as manufacturer,
            sum("Landing Count") as count
        from "DKU_TUTORIAL_SQLINPYTHON_sfo_prepared"
        group by "Aircraft Manufacturer"
        order by count desc limit 5
