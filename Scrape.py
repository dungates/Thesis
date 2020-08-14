import tabula
if __name__ == '__main__':
    # Read pdf into DataFrame
    # df = tabula.read_pdf("./Credit Suisse Reports/2018GlobalWealthData.pdf", pages='114-117')
    # print(df)

    # convert PDF into CSV
    tabula.convert_into("./Credit Suisse Reports/2018GlobalWealthData.pdf", "output.csv", output_format="csv", pages='114-117')

    # # convert all PDFs in a directory
    # tabula.convert_into_by_batch("input_directory", output_format='csv', pages='all')






