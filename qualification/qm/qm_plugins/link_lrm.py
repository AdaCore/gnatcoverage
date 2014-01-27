
LRMREF = "LRMREF:"


def parse_req(artifact):
    """
    Parses the requirements to extract LRM Sections
    that are bound "from" of links 'Lrm_Req' defined in model
    """

    loc = artifact.resources[0][0]
    ref_list = []

    for line in loc.get_content().splitlines():

        if line.lstrip().startswith(LRMREF):
            ref_list = ['/%s' % k.strip()
                        for k in line.split(LRMREF, 1)[1].split(',')]

    return ref_list
